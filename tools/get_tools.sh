#!/usr/bin/env bash
#
# ------------------------------------------------------------------------------
# This script downloads and sets up the following tools:
#   - [JDK 8u292-b10](https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/tag/jdk8u292-b10)
#   - [Apache Maven v3.8.4](https://maven.apache.org/index.html)
#   - [Set of non-trivial classes from the original DynaMOSA study](https://github.com/jose/non-trivial-java-classes-to-study-search-based-software-testing-approaches)
#   - [SF110](https://www.evosuite.org/experimental-data/sf110)
#   - [EvoSuite](https://github.com/jose/smell-free-tests-evosuite.git)
#   - [R](https://www.r-project.org)
#
# Usage:
# get_tools.sh
#
# ------------------------------------------------------------------------------

SCRIPT_DIR=$(cd `dirname $0` && pwd)

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

MVN_VERSION="3.8.4"
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
# Download classes from the DynaMOSA study
#

echo ""
echo "Setting up classes from the DynaMOSA study..."

DYNAMOSA_STUDY_CLASSES_REPO_DIR="$SCRIPT_DIR/non-trivial-java-classes-to-study-search-based-software-testing-approaches"
DYNAMOSA_STUDY_CLASSES_DIR="$SCRIPT_DIR/dynamosa-study-classes"

# remove any previous file and directory
rm -rf "$DYNAMOSA_STUDY_CLASSES_REPO_DIR" "$DYNAMOSA_STUDY_CLASSES_DIR"

git clone https://github.com/jose/non-trivial-java-classes-to-study-search-based-software-testing-approaches.git
if [ $? -ne 0 ] || [ ! -d "$DYNAMOSA_STUDY_CLASSES_REPO_DIR" ]; then
  die "[ERROR] Failed to clone of 'DynaMOSA' study's classes!"
fi

pushd . > /dev/null 2>&1
cd "$DYNAMOSA_STUDY_CLASSES_REPO_DIR"
  # Switch to a specific commit
  git checkout 563e99418fa5b02b1665a57b9356d6e98d456fbd
popd > /dev/null 2>&1

mv -f "$DYNAMOSA_STUDY_CLASSES_REPO_DIR/subjects" "$DYNAMOSA_STUDY_CLASSES_DIR" || die "[ERROR] Failed to move $DYNAMOSA_STUDY_CLASSES_REPO_DIR/subjects to $DYNAMOSA_STUDY_CLASSES_DIR!"

rm -rf "$DYNAMOSA_STUDY_CLASSES_REPO_DIR" # clean up

#
# Download SF110 subjects
#

echo ""
echo "Setting up SF110 subjects..."

SF110_VERSION="20130704"
SF110_FILE="SF110-$SF110_VERSION.zip"
SF110_URL="http://www.evosuite.org/files/$SF110_FILE"
SF110_TMP_DIR="$SCRIPT_DIR/SF110-20130704"
SF110_DIR="$SCRIPT_DIR/sf110"

# remove any previous file or directory
rm -rf "$SCRIPT_DIR/$SF110_FILE" "$SF110_TMP_DIR" "$SF110_DIR"

wget -np -nv "$SF110_URL" -O "$SCRIPT_DIR/$SF110_FILE"
if [ $? -ne 0 ] || [ ! -s "$SCRIPT_DIR/$SF110_FILE" ]; then
  die "[ERROR] Failed to download $SF110_URL!"
fi

pushd . > /dev/null 2>&1
cd "$SCRIPT_DIR"
  unzip "$SF110_FILE" # extract it
  if [ "$?" -ne "0" ] || [ ! -d "$SF110_TMP_DIR" ]; then
    die "[ERROR] Failed to extract $SCRIPT_DIR/$SF110_FILE!"
  fi

  # Fix missing files in the SF110 package
  mkdir "$SF110_TMP_DIR/27_gangup/native" "$SF110_TMP_DIR/110_firebird/native"
  cp -Rv $DYNAMOSA_STUDY_CLASSES_DIR/27_gangup/native/*    "$SF110_TMP_DIR/27_gangup/native/"
  cp -Rv $DYNAMOSA_STUDY_CLASSES_DIR/110_firebird/native/* "$SF110_TMP_DIR/110_firebird/native/"
popd > /dev/null 2>&1

mv -f "$SF110_TMP_DIR" "$SF110_DIR" || die "[ERROR] Failed to move $SF110_TMP_DIR to $SF110_DIR!"

rm -f "$SCRIPT_DIR/$SF110_FILE" # clean up

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
  # Switch to 'develop_SIAs' branch
  git checkout smell-free-tests || die "[ERROR] Branch 'smell-free-tests' not found!"
  # Switch to a specific commit
  git checkout 3aa1b21d1efafbe34f1cf5d5295e5e743827dc1e
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

Rscript "$SCRIPT_DIR/get_libraries.R" || die "[ERROR] Failed to install/load all required R packages!"

echo ""
echo "DONE! All tools have been successfully prepared."

# EOF
