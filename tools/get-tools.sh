#!/usr/bin/env bash
#
# ------------------------------------------------------------------------------
# This script downloads and sets up the following tools:
#   - [JDK 8u292-b10](https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/tag/jdk8u292-b10)
#   - [Apache Maven v3.8.5](https://maven.apache.org/index.html)
#   - [Set of non-trivial classes from the original DynaMOSA study](https://github.com/jose/non-trivial-java-classes-to-study-search-based-software-testing-approaches)
#   - [EvoSuite](https://github.com/jose/smell-free-tests-evosuite.git)
#   - [R](https://www.r-project.org)
#
# Usage:
# get-tools.sh
#
# ------------------------------------------------------------------------------

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"

export _JAVA_OPTIONS="-Xmx4096M -XX:MaxHeapSize=1024M"
export MAVEN_OPTS="-Xmx1024M"
export ANT_OPTS="-Xmx2048M -XX:MaxHeapSize=1024M"

#
# Print error message to the stdout and exit.
#
die() {
  echo "$@" >&2
  exit 1
}

# ------------------------------------------------------------------------- Deps

# Check whether 'wget' is available
wget --version > /dev/null 2>&1 || die "[ERROR] Could not find 'wget' to download all dependencies. Please install 'wget' and re-run the script."

# Check whether 'git' is available
git --version > /dev/null 2>&1 || die "[ERROR] Could not find 'git' to clone git repositories. Please install 'git' and re-run the script."

# ------------------------------------------------------------------------- Main

OS_NAME=$(uname -s | tr "[:upper:]" "[:lower:]")
OS_ARCH=$(uname -m | tr "[:upper:]" "[:lower:]")

[[ $OS_NAME == *"linux"* ]] || die "[ERROR] All scripts have been developed and tested on Linux, and as we are not sure they will work on other OS, we can continue running this script."

#
# Download JDK...
#

echo ""
echo "Setting up JDK..."

JDK_VERSION="8u292"
JDK_BUILD_VERSION="b10"
JDK_FILE="OpenJDK8U-jdk_x64_linux_hotspot_${JDK_VERSION}${JDK_BUILD_VERSION}.tar.gz"
JDK_TMP_DIR="$SCRIPT_DIR/jdk$JDK_VERSION-$JDK_BUILD_VERSION"
JDK_DIR="$SCRIPT_DIR/jdk-8"
JDK_URL="https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk$JDK_VERSION-$JDK_BUILD_VERSION/$JDK_FILE"

# remove any previous file or directory
rm -rf "$SCRIPT_DIR/$JDK_FILE" "$JDK_TMP_DIR" "$JDK_DIR"

# get file
wget -np -nv "$JDK_URL" -O "$SCRIPT_DIR/$JDK_FILE"
if [ $? -ne 0 ] || [ ! -s "$SCRIPT_DIR/$JDK_FILE" ]; then
  die "[ERROR] Failed to download $JDK_URL!"
fi

# unpack file
pushd . > /dev/null 2>&1
cd "$SCRIPT_DIR"
  tar -xvzf "$JDK_FILE" # extract it
  if [ "$?" -ne "0" ] || [ ! -d "$JDK_TMP_DIR" ]; then
    die "[ERROR] Failed to extract $SCRIPT_DIR/$JDK_FILE!"
  fi
popd > /dev/null 2>&1

mv -f "$JDK_TMP_DIR" "$JDK_DIR" || die "[ERROR] Failed to move $JDK_TMP_DIR to $JDK_DIR!"

# Set Java HOME for subsequent commands
export JAVA_HOME="$JDK_DIR"
export PATH="$JAVA_HOME/bin:$PATH"

# Check whether 'javac' is available
javac -version > /dev/null 2>&1 || die "[ERROR] Failed to find the javac executable."

rm -f "$SCRIPT_DIR/$JDK_FILE" # clean up

#
# Download Apache Maven
#

echo ""
echo "Setting up Maven..."

MVN_VERSION="3.8.5"
MVN_FILE="apache-maven-$MVN_VERSION-bin.zip"
MVN_URL="https://dlcdn.apache.org/maven/maven-3/$MVN_VERSION/binaries/$MVN_FILE"
MVN_TMP_DIR="$SCRIPT_DIR/apache-maven-$MVN_VERSION"
MVN_DIR="$SCRIPT_DIR/apache-maven"

# remove any previous file or directory
rm -rf "$SCRIPT_DIR/$MVN_FILE" "$MVN_TMP_DIR" "$MVN_DIR"

# get file
wget -np -nv "$MVN_URL" -O "$SCRIPT_DIR/$MVN_FILE"
if [ $? -ne 0 ] || [ ! -s "$SCRIPT_DIR/$MVN_FILE" ]; then
  die "[ERROR] Failed to download $MVN_URL!"
fi

# unpack file
pushd . > /dev/null 2>&1
cd "$SCRIPT_DIR"
  unzip "$MVN_FILE" # extract it
  if [ "$?" -ne "0" ] || [ ! -d "$MVN_TMP_DIR" ]; then
    die "[ERROR] Failed to extract $SCRIPT_DIR/$MVN_FILE!"
  fi
popd > /dev/null 2>&1

mv -f "$MVN_TMP_DIR" "$MVN_DIR" || die "[ERROR] Failed to move $MVN_TMP_DIR to $MVN_DIR!"

# Add Apache Maven to the PATH for subsequent commands
export PATH="$MVN_DIR/bin:$PATH"
# Check whether 'mvn' is available
mvn -version > /dev/null 2>&1 || die "[ERROR] Failed to find the mvn executable."

rm -f "$SCRIPT_DIR/$MVN_FILE" # clean up

#
# Download projects/classes from the DynaMOSA study
#

echo ""
echo "Setting up projects/classes from the DynaMOSA study..."

DYNAMOSA_STUDY_PROJECTS_DIR="$SCRIPT_DIR/dynamosa-study-projects"

# remove any previous file and directory
rm -rf "$DYNAMOSA_STUDY_PROJECTS_DIR"

git clone https://github.com/jose/non-trivial-java-classes-to-study-search-based-software-testing-approaches.git "$DYNAMOSA_STUDY_PROJECTS_DIR"
if [ $? -ne 0 ] || [ ! -d "$DYNAMOSA_STUDY_PROJECTS_DIR" ]; then
  die "[ERROR] Failed to clone of 'DynaMOSA' study's classes!"
fi

pushd . > /dev/null 2>&1
cd "$DYNAMOSA_STUDY_PROJECTS_DIR"
  # Switch to a specific commit
  git checkout 64049ead11feb0c668fc7d27f375b280dc290c72
popd > /dev/null 2>&1

#
# Get EvoSuite
#

echo ""
echo "Setting up EvoSuite..."

EVOSUITE_DIR="$SCRIPT_DIR/evosuite"
EVOSUITE_JAR="$SCRIPT_DIR/evosuite.jar"
EVOSUITE_GEN_JAR="$EVOSUITE_DIR/master/target/evosuite-master-1.2.1-SNAPSHOT.jar"

# remove any previous file and directory
rm -rf "$EVOSUITE_DIR" "$EVOSUITE_JAR"

git clone https://github.com/jose/smell-free-tests-evosuite.git "$EVOSUITE_DIR"
if [ $? -ne 0 ] || [ ! -d "$EVOSUITE_DIR" ]; then
  die "[ERROR] Failed to clone EvoSuite's repository!"
fi

pushd . > /dev/null 2>&1
cd "$EVOSUITE_DIR"
  # Switch to 'smell-free-tests' branch
  git checkout smell-free-tests || die "[ERROR] Branch 'smell-free-tests' not found!"
  # Switch to a specific commit
  git checkout f2cceae80eab1a8a2d26a6d40197d2f8ef7cb3df || die "[ERROR] Failed to checkout commit f2cceae80eab1a8a2d26a6d40197d2f8ef7cb3df!"
  # Compile EvoSuite
  mvn clean package -DskipTests=true || die "[ERROR] Failed to package EvoSuite!"
popd > /dev/null 2>&1

[ -s "$EVOSUITE_GEN_JAR" ] || die "[ERROR] $EVOSUITE_GEN_JAR does not exist or it is empty!"
# Place 'evosuite.jar'
ln -s "$EVOSUITE_GEN_JAR" "$EVOSUITE_JAR" || die "[ERROR] Failed to create $EVOSUITE_JAR!"
[ -s "$EVOSUITE_JAR" ] || die "[ERROR] $EVOSUITE_JAR does not exist or it is empty!"

#
# R packages
#

echo ""
echo "Setting up R..."

# Check whether 'Rscript' is available
Rscript --version > /dev/null 2>&1 || die "[ERROR] Could not find 'Rscript' to install R's packages!"

Rscript "$SCRIPT_DIR/get-libraries.R" "$SCRIPT_DIR" || die "[ERROR] Failed to install/load all required R packages!"

echo ""
echo "DONE! All tools have been successfully prepared."

# EOF
