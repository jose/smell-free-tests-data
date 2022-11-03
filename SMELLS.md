\ifdefined\modoTese
\subsection*{1 -- Abnormal UTF-Use (AUU)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:auu}
\else
\subsubsection*{1 -- Abnormal UTF-Use (AUU)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:auu}
\fi
%
\noindent\textit{Description:} Test suites change the default behavior of the unit-testing framework.

\noindent\textit{Impact:} Harder to understand and maintain.

\ifdefined\modoTese
\subsection*{2 -- Anonymous Test (AT)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:at}
\else
\subsubsection*{2 -- Anonymous Test (AT)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:at}
\fi
%
\noindent\textit{Description:} Test cases with non-descriptive names.

\noindent\textit{Impact:} Harder to understand and maintain.

\noindent\textit{Alternative Designations:} Test-Method Category Name~\cite{reichhart2007rule}.

%\noindent\textit{Detection Strategy:} Check whether the name of a test case follows a specific pattern~\cite{reichhart2007rule}.
%
%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{grano2019scented}.,label=exampleAnonymousTest]
% @Test
%public void test12 () throws Throwable {
%  JSTerm jSTerm0 = new JSTerm();
%  jSTerm0.makeVariable();
%  jSTerm0.add((Object) "");
%  jSTerm0.matches(jSTerm0);
%  assertEquals(false, jSTerm0.isGround());
%  assertEquals(true, jSTerm0.isVariable());
%}
%\end{lstlisting}

\ifdefined\modoTese
\subsection*{3 -- Assertion Roulette (AR)~\cite{grano2019scented, meszaros2007xunit, tsDetectWebsite, spadini2020investigating, van2001refactoring}:}\label{smell:ar}
\else
\subsubsection*{3 -- Assertion Roulette (AR)~\cite{grano2019scented, meszaros2007xunit, tsDetectWebsite, spadini2020investigating, van2001refactoring}:}\label{smell:ar}
\fi
%
\noindent\textit{Description:} A test case has several unexplained assertions. This test smell can arise for one of two reasons:
%
\begin{enumerate}

  \item A test case has an excessive number of assertions (typically because the test case is inspecting too much functionality);
  
  \item A test case has multiple assertions without assertion messages\footnote{An assertion message is an optional argument in an assertion statement that defines a message to be displayed when the assertion fails.}.
  
\end{enumerate}

\noindent\textit{Impact:} When the test fails, it is difficult to identify the exact assertion that failed (hinders comprehensibility and maintainability).

%\noindent\textit{Detection:} Verify if a test case has two or more assertions without assertions messages~\cite{tsDetectWebsite}.

%There are several unexplained assertions in a test case. This smell can occur for one of two reasons:

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{palomba2016diffusion}.,label=exampleAssertionRoulette]
\begin{lstlisting}[language=Java,caption=Example of a test case with the ``Assertion Roulette'' test smell~\protect\cite{palomba2016diffusion}.,label=exampleAssertionRoulette]
@Test
public void test8() throws Throwable {
  Document document0 = new Document("", "");
  assertNotNull(document0);
  
  document0.procText.add((Character) 's');
  String string0 = document0.stringify();
  assertEquals("s", document0.stringify());
  assertNotNull(string0);
  assertEquals("s", string0);
}
\end{lstlisting}

\ifdefined\modoTese
\subsection*{4 -- Brittle Assertion (BA)~\cite{aljedaani2021test, huo2014improving}:}\label{smell:ba}
\else
\subsubsection*{4 -- Brittle Assertion (BA)~\cite{aljedaani2021test, huo2014improving}:}\label{smell:ba}
\fi
%
\noindent\textit{Description:} One or more assertions in a test case check values that said test case does not manipulate (the test is checking too much).

%Assertions check values not manipulated by a test case (the test case is checking too much).

%A test case contains assertions that check values not manipulated by the test (the test case is checking too much).

\noindent\textit{Impact:} Less effective (the test may fail when it should not) and harder to maintain.

%\noindent\textit{Detection Strategy:} Verify if a test case contains assertions that check values not manipulated by the test~\cite{huo2014improving}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{huo2014improving}.,label=exampleBrittleAssertion]
%\begin{lstlisting}[language=Java,caption=Example --- Brittle Assertion~\protect\cite{huo2014improving}.,label=exampleBrittleAssertion]
% @Test
%public void testPut() {
%  String newValue = "ABC";
%  assertEquals(stringVal, modifiableMap.put(stringProp.getName(), newValue);
%  assertEquals(newValue, dynaBean.get(stringProp.getName()));
%  assertEquals(newValue, modifiableMap.get(stringProp.getName()));
%}
%\end{lstlisting}

\ifdefined\modoTese
\subsection*{5 -- Conditional Test Logic (CTL)~\cite{aljedaani2021test, meszaros2007xunit, peruma2019distribution, tsDetectWebsite, spadini2020investigating}:}\label{smell:ctl}
\else
\subsubsection*{5 -- Conditional Test Logic (CTL)~\cite{aljedaani2021test, meszaros2007xunit, peruma2019distribution, tsDetectWebsite, spadini2020investigating}:}\label{smell:ctl}
\fi
%
\noindent\textit{Description:} Test cases have control structures that may prevent the execution of specific statements (i.e., the test cases have many execution paths).

\noindent\textit{Impact:} Behave unpredictably and are less effective (this smell may hamper test coverage and fault detection effectiveness). Such tests are also harder to understand and maintain.

\noindent\textit{Alternative Designations:} Indented Test~\cite{breugelmans2008testq, meszaros2007xunit} and Guarded Test~\cite{reichhart2007rule}.

\noindent\textit{Variants:} Control Logic (ConL)~\cite{aljedaani2021test, reichhart2007rule} --- test cases use methods such as debug or halt to control the execution flow.

\ifdefined\modoTese
\subsection*{6 -- Constructor Initialization (CI)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite, virginio2019influence}:}\label{smell:ci}
\else
\subsubsection*{6 -- Constructor Initialization (CI)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite, virginio2019influence}:}\label{smell:ci}
\fi
%
\noindent\textit{Description:} A test suite uses a constructor to initialize the fields; instead, tests should have set up methods (using constructors is a bad practice and, as such, should be avoided).

%A test suite uses a constructor to initialize the fields. Instead of a constructor, test suites should have set up methods (using constructors is a bad practice and, as such, should be avoided).

\noindent\textit{Impact:} Harder to understand.

%\subsubsection{Control Logic (ConL)~\cite{reichhart2007rule, aljedaani2021test}}\label{smell:conl}
%
%Test cases control the execution flow by using methods such as debug or halt.

\ifdefined\modoTese
\subsection*{7 -- Dead Field (DF)~\cite{aljedaani2021test, greiler2013automated}:}\label{smell:df}
\else
\subsubsection*{7 -- Dead Field (DF)~\cite{aljedaani2021test, greiler2013automated}:}\label{smell:df}
\fi
%
\noindent\textit{Description:} Fields are initialized in an implicit setup but not used by the test cases.

\noindent\textit{Impact:} This smell affects understandability and leads to slower tests (unnecessary work).

\noindent\textit{Alternative Designations:} Unused Shared-Fixture Variables~\cite{reichhart2007rule}.

\ifdefined\modoTese
\subsection*{8 -- Default Test (DT)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite}:}\label{smell:dt}
\else
\subsubsection*{8 -- Default Test (DT)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite}:}\label{smell:dt}
\fi
%
\noindent\textit{Description:} Using the example test suites automatically generated by Android Studio.

%(these tests should only be used as examples).

\noindent\textit{Impact:} If not removed, developers may add test cases to such test suites.

%Problems can also arise when renaming these test suites (i.e., affects maintainability).

\ifdefined\modoTese
\subsection*{9 -- Duplicate Assert (DA)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite, virginio2019influence}:}\label{smell:da}
\else
\subsubsection*{9 -- Duplicate Assert (DA)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite, virginio2019influence}:}\label{smell:da}
\fi
%
\noindent\textit{Description:} A test case contains several assertions that check the same condition (i.e., a test case contains two or more assertions with the same parameters). This smell arises even if the test case checks the same condition with different values.

\noindent\textit{Impact:} Harder to understand and maintain.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{peruma2019distribution}.,label=exampleDuplicateAssert]
\begin{lstlisting}[language=Java,caption=Example of a test case with the ``Duplicate Assert'' test smell~\protect\cite{peruma2019distribution}.,label=exampleDuplicateAssert]
@Test
public void testXmlSanitizer() {
  .....
  valid = XmlSanitizer.isValid("Fritz-box");
  assertEquals("Minus is valid", true, valid);
  System.out.println("Minus test - passed");
  valid = XmlSanitizer.isValid ("Fritz-box");
  assertEquals("Minus is valid", true, valid);
  System.out.println("Minus test - passed");
  .....
}
\end{lstlisting}

\ifdefined\modoTese
\subsection*{10 -- Eager Test (ET)~\cite{meszaros2007xunit, Panichella2020, tsDetectWebsite, spadini2020investigating, van2001refactoring}:}\label{smell:et}
\else
\subsubsection*{10 -- Eager Test (ET)~\cite{meszaros2007xunit, Panichella2020, tsDetectWebsite, spadini2020investigating, van2001refactoring}:}\label{smell:et}
\fi
%
\noindent\textit{Description:} A test case checks multiple methods of the class under test (i.e., it verifies too much functionality). Typically, this smell is said to occur when a test case checks two or more methods of the class under test.

\noindent\textit{Impact:} Harder to understand and maintain.

%\noindent\textit{Detection:} Verify if a test case calls more than one method of the class under test~\cite{tsDetectWebsite}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{Panichella2020}.,label=exampleEagerTest]
\begin{lstlisting}[language=Java,caption=Example of a test case with the ``Eager Test'' test smell~\protect\cite{Panichella2020}.,label=exampleEagerTest]
@Test
public void test56() throws Throwable {
  SubstringLabeler substringLabeler0 = new SubstringLabeler();
  substringLabeler0.connectionNotification("testSet", "testSet");
  InstanceEvent instanceEvent0 = substringLabeler0.m_ie;
  substringLabeler0.acceptInstance(instanceEvent0);
  assertEquals("SubstringLabeler", substringLabeler0.getCustomName());
  assertFalse(substringLabeler0.isBusy());
  assertEquals("Match", substringLabeler0.getMatchAttributeName());
}
\end{lstlisting}

%\subsubsection{Empty Method Category (EMC)~\cite{reichhart2007rule, aljedaani2021test}}\label{smell:emc}
%A test case with an empty method category.

\ifdefined\modoTese
\subsection*{11 -- Empty Shared-Fixture (ESF)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:esf}
\else
\subsubsection*{11 -- Empty Shared-Fixture (ESF)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:esf}
\fi
%
\noindent\textit{Description:} A test suite contains an implicit setup with an empty body.

\noindent\textit{Impact:} Harder to understand and maintain.

\ifdefined\modoTese
\subsection*{12 -- Empty Test (EmT)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite, spadini2020investigating, virginio2019influence}:}\label{smell:emt}
\else
\subsubsection*{12 -- Empty Test (EmT)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite, spadini2020investigating, virginio2019influence}:}\label{smell:emt}
\fi
%
\noindent\textit{Description:} Test cases without executable statements.

\noindent\textit{Impact:} Less effective (empty tests always pass, thus giving a false sense of security).

\noindent\textit{Variants:} Comments Only Test (COT)~\cite{reichhart2007rule} --- corresponds to a commented-out test case.

%\noindent\textit{Detection Strategy:} Check whether a test case contains no executable statements~\cite{tsDetectWebsite}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{peruma2019distribution}.,label=exampleEmptyTest]
%\begin{lstlisting}[language=Java,caption=Example --- Empty Test~\protect\cite{peruma2019distribution}.,label=exampleEmptyTest]
%/* ** Test method without executable statements ** */
% @Test
%public void testCredGetFullSampleV1() throws Throwable {
%// ScrapedCredentials credentials = innerCredTest (FULL_SAMPLE_v1);
%// assertEquals ("p4ssw0rd", credentials.pass );
%// assertEquals ("user@example.com" , credentials.user );
%}
%\end{lstlisting}

%\subsubsection{Empty Test-Method Category (ETMC)~\cite{reichhart2007rule, aljedaani2021test}}\label{smell:etmc}
%A test case with an empty test method category.

\ifdefined\modoTese
\subsection*{13 -- Erratic Test (ErT)~\cite{meszaros2007xunit}:}\label{smell:ert}
\else
\subsubsection*{13 -- Erratic Test (ErT)~\cite{meszaros2007xunit}:}\label{smell:ert}
\fi
%
\noindent\textit{Description:} Tests exhibit erratic behavior --- they might give different results in different runs (e.g., different people run the same tests but obtain different results).

\noindent\textit{Impact:} Less effective.

%The same tests can give different results in different runs (e.g., different people run the same tests but obtain different results); thus, the tests become less effective.

%\noindent\textit{Detection Strategy:} Run the same test in different contexts to verify whether there is any behavior change~\cite{meszaros2007xunit}.

\ifdefined\modoTese
\subsection*{14 -- Exception Handling (EH)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite}:}\label{smell:eh}
\else
\subsubsection*{14 -- Exception Handling (EH)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite}:}\label{smell:eh}
\fi
%
\noindent\textit{Description:} Tests use throw/catch statements instead of JUnit's exception handling.

\noindent\textit{Impact:} Harder to understand and maintain.  This smell also makes tests less effective.

\ifdefined\modoTese
\subsection*{15 -- For Testers Only (FTO)~\cite{bavota2015test, bavota2012empirical, grano2019scented, meszaros2007xunit, van2001refactoring}:}\label{smell:fto}
\else
\subsubsection*{15 -- For Testers Only (FTO)~\cite{bavota2015test, bavota2012empirical, grano2019scented, meszaros2007xunit, van2001refactoring}:}\label{smell:fto}
\fi
%
\noindent\textit{Description:} The production class contains code exclusively used by tests.

\noindent\textit{Impact:} This test smell negatively impacts the understandability and maintainability of production code since it makes the system under test more complex.

%\noindent\textit{Variants:} Mixed Selectors (MS)~\cite{reichhart2007rule} – corresponds to classes that contain both production methods and test cases.

\ifdefined\modoTese
\subsection*{16 -- Fragile Test (FT)~\cite{meszaros2007xunit}:}\label{smell:ft}
\else
\subsubsection*{16 -- Fragile Test (FT)~\cite{meszaros2007xunit}:}\label{smell:ft}
\fi
%
\noindent\textit{Description:} Tests start to fail to compile/run due to unrelated changes to the system under test. Fragile tests can also start to fail in situations where nothing was changed.

\noindent\textit{Impact:} This smell leads to increased maintenance costs.

%\noindent\textit{Detection Strategy:} Check whether the test case is testing only that which is necessary~\cite{meszaros2007xunit}.

\ifdefined\modoTese
\subsection*{17 -- Frequent Debugging (FD)~\cite{meszaros2007xunit}:}\label{smell:fd}
\else
\subsubsection*{17 -- Frequent Debugging (FD)~\cite{meszaros2007xunit}:}\label{smell:fd}
\fi
%
\noindent\textit{Description:} It is often necessary to manually debug the tests to determine the cause of failures. This test smell might arise due to (1) the lack of available information or (2) infrequently run tests.

\noindent\textit{Impact:} Such tests are less effective and harder to understand and maintain.

\noindent\textit{Alternative Designations:} Manual Debugging~\cite{meszaros2007xunit}.

%\noindent\textit{Detection Strategy:} The assertion messages in a test case are not descriptive enough~\cite{meszaros2007xunit}.

\ifdefined\modoTese
\subsection*{18 -- General Fixture (GF)~\cite{bavota2015test, greiler2013automated, meszaros2007xunit, spadini2020investigating, van2001refactoring}:}\label{smell:gf}
\else
\subsubsection*{18 -- General Fixture (GF)~\cite{bavota2015test, greiler2013automated, meszaros2007xunit, spadini2020investigating, van2001refactoring}:}\label{smell:gf}
\fi
%
\noindent\textit{Description:} An implicit setup is excessively general/large, so the test cases do not access the entirety of the fixture.

\noindent\textit{Impact:} This smell affects comprehensibility and causes tests to run slower due to the unnecessary extra work. It can also make the tests fragile (i.e., affects maintainability).

\ifdefined\modoTese
\subsection*{19 -- Hard-to-Test Code (HTTC)~\cite{meszaros2007xunit}:}\label{smell:httc}
\else
\subsubsection*{19 -- Hard-to-Test Code (HTTC)~\cite{meszaros2007xunit}:}\label{smell:httc}
\fi
%
\noindent\textit{Description:} The system under test has properties that make it inherently difficult to test (e.g., highly coupled code). It is also possible to have test code that is difficult to test.

\noindent\textit{Impact:} Less effective automatically generated tests. Harder to manually write tests.

\ifdefined\modoTese
\subsection*{20 -- Ignored Test (IgT)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite, spadini2020investigating, virginio2019influence}:}\label{smell:igt}
\else
\subsubsection*{20 -- Ignored Test (IgT)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite, spadini2020investigating, virginio2019influence}:}\label{smell:igt}
\fi
%
\noindent\textit{Description:} Test case/suite uses the \emph{@Ignore} annotation, thus impeding it from running.

\noindent\textit{Impact:} Test suites become harder to understand. Causes compilation time overhead.

\ifdefined\modoTese
\subsection*{21 -- Indirect Testing (IT)~\cite{bavota2015test, grano2019scented, meszaros2007xunit, Panichella2020, van2001refactoring}:}\label{smell:it}
\else
\subsubsection*{21 -- Indirect Testing (IT)~\cite{bavota2015test, grano2019scented, meszaros2007xunit, Panichella2020, van2001refactoring}:}\label{smell:it}
\fi
%
\noindent\textit{Description:} A test case performs tests on classes other than the one under test. This smell may arise because the test case is (indirectly) checking the respective production class using methods of other classes.

\noindent\textit{Impact:} This smell negatively affects the comprehensibility and maintainability of test cases. It can also hamper the debugging process.

%\noindent\textit{Detection:} Check whether a test case calls one or more methods of classes other than the class under test~\cite{bavota2015test}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{Panichella2020}.,label=exampleIndirectTesting]
%\begin{lstlisting}[language=Java,caption=Example --- Indirect Testing~\protect\cite{Panichella2020}.,label=exampleIndirectTesting]
% @Test
%public void test05() throws Throwable {
%  LinkedHashMap<String, Object> linkedHashMap0 = new LinkedHashMap<String,Object>();
%  TeamFinderImpl teamFinderImpl0 = new TeamFinderImpl();
%  linkedHashMap0.put("com.liferay.portal.service.persistence.TeamFinder.findByG_N_D",teamFinderImpl0);
%  teamFinderImpl0.setJoin((QueryPos) null, linkedHashMap0);
%  assertFalse(linkedHashMap0.isEmpty());
%}
%\end{lstlisting}

\begin{lstlisting}[language=Java,caption=Example of a test case with the ``Indirect Testing'' test smell~\protect\cite{palomba2016diffusion}.,label=exampleIndirectTesting]
@Test
public void test3() throws Throwable {
  SweetHome3D sweetHome3D0 = new SweetHome3D();
  HomeRecorder.Type homeRecorder_Type0 = HomeRecorder.Type.DEFAULT;
  HomeFileRecorder homeFileRecorder0 = (HomeFileRecorder) sweetHome3D0.getHomeRecorder(homeRecorder_Type0);
  assertNotNull(homeFileRecorder0);
}
\end{lstlisting}

\ifdefined\modoTese
\subsection*{22 -- Lack of Cohesion of Methods (LCM)~\cite{aljedaani2021test, greiler2013automated}:}\label{smell:lcm}
\else
\subsubsection*{22 -- Lack of Cohesion of Methods (LCM)~\cite{aljedaani2021test, greiler2013automated}:}\label{smell:lcm}
\fi
%
\noindent\textit{Description:} Unrelated test cases are arranged into a test suite (i.e., they are not cohesive).

\noindent\textit{Impact:} Harder to understand and maintain.

%\noindent\textit{Detection:} \todo{!} A test suite contains test cases that perform tests on different objects: use a metric to measure the cohesion between the test cases in a test suite~\cite{greiler2013automated}.

\begin{lstlisting}[language=Java,caption=Example of a test suite with the ``Lack of Cohesion of Methods'' test smell.,label=exampleLackOfCohesionOfMethods]  
@Test
public void test00() throws Throwable {
  Login login0 = new Login();
  int int0 = login0.getAuth_num();
  assertEquals(0, login0.getAuth_max());
  assertEquals(0, int0);
}

@Test
public void test01() throws Throwable {
  int int0 = UserManagement.getNBGM();
  assertEquals(0, int0);
}
\end{lstlisting}

%\begin{lstlisting}[language=Java,caption=Example --- Lack of Cohesion of Methods.,label=exampleLackOfCohesionOfMethods]
%public class Login_ESTest extends Login_ESTest_scaffolding {
%  @Test
%  public void test00() throws Throwable {
%    Login login0 = new Login();
%    int int0 = login0.getAuth_num();
%    assertEquals(0, login0.getAuth_max());
%    assertEquals(0, int0);
%  }
%  
%  @Test
%  public void test01() throws Throwable {
%    int int0 = UserManagement.getNBGM();
%    assertEquals(0, int0);
%  }
%}
%\end{lstlisting}

\ifdefined\modoTese
\subsection*{23 -- Lazy Test (LT)~\cite{aljedaani2021test, bavota2015test, bavota2012empirical, tsDetectWebsite, van2001refactoring}:}\label{smell:lt}
\else
\subsubsection*{23 -- Lazy Test (LT)~\cite{aljedaani2021test, bavota2015test, bavota2012empirical, tsDetectWebsite, van2001refactoring}:}\label{smell:lt}
\fi
%
\noindent\textit{Description:} Multiple test cases in a test suite check the same production method.

\noindent\textit{Impact:} This smell can hinder maintainability.

%\noindent\textit{Detection:} Check whether two or more test cases from the same test suite call the same production method~\cite{tsDetectWebsite}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{tsDetectWebsite}.,label=exampleLazyTest]
\begin{lstlisting}[language=Java,caption=Example of a test suite with the ``Lazy Test'' test smell~\protect\cite{tsDetectWebsite}.,label=exampleLazyTest]
@Test
public void testDecrypt() throws Exception {
  FileInputStream file = new FileInputStream(ENCRYPTED_DATA_FILE_4_14);
  byte[] enfileData = new byte[file.available()];
  FileInputStream input = new FileInputStream(DECRYPTED_DATA_FILE_4_14);
  byte[] fileData = new byte[input.available()];
  input.read(fileData);
  input.close();
  file.read(enfileData);
  file.close();
  String expectedResult = new String(fileData, "UTF-8");
  assertEquals("Testing simple decrypt",expectedResult, Cryptographer.decrypt(enfileData, "test"));
}

@Test
public void testEncrypt() throws Exception {
  String xml = readFileAsString(DECRYPTED_DATA_FILE_4_14);
  byte[] encrypted = Cryptographer.encrypt(xml, "test");
  String decrypt = Cryptographer.decrypt(encrypted, "test");
  assertEquals(xml, decrypt);
}
\end{lstlisting}

\ifdefined\modoTese
\subsection*{24 -- Likely Ineffective Object-Comparison (LIOC)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:lioc}
\else
\subsubsection*{24 -- Likely Ineffective Object-Comparison (LIOC)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:lioc}
\fi
%
\noindent\textit{Description:} A test case has one or more object comparisons that will never fail (e.g., a test case compares an object with itself).

\noindent\textit{Impact:} Less effective.

%\noindent\textit{Detection:} A test case compares an object with itself.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{Panichella2020}.,label=exampleLikelyIneffectiveObjectComparison]
\begin{lstlisting}[language=Java,caption=Example of a test case with the ``Likely Ineffective Object-Comparison'' test smell~\protect\cite{Panichella2020}.,label=exampleLikelyIneffectiveObjectComparison]
@Test
public void test58() throws Throwable {
  OrganizationImpl organizationImpl0 = new OrganizationImpl();
  boolean boolean0 = organizationImpl0.equals(organizationImpl0);
  assertTrue(boolean0);
  assertEquals(0L, organizationImpl0.getPrimaryKey());
}
\end{lstlisting}

\ifdefined\modoTese
\subsection*{25 -- Magic Number Test (MNT)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite, spadini2020investigating, virginio2019influence}:}\label{smell:mnt}
\else
\subsubsection*{25 -- Magic Number Test (MNT)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite, spadini2020investigating, virginio2019influence}:}\label{smell:mnt}
\fi
%
\noindent\textit{Description:} A test case uses unexplained/undocumented numerical values.

\noindent\textit{Impact:} This test smell hampers the understandability and maintainability of test cases (it is harder to understand the meaning and purpose of such values).

%\noindent\textit{Detection Strategy:} Verify if a test case uses numerical literals as parameters~\cite{peruma2019distribution}.
%
%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{peruma2019distribution}.,label=exampleMagicNumberTest]
%@Test
%public void testGetLocalTimeAsCalendar () {
%  Calendar localTime = calc.getLocalTimeAsCalendar(BigDecimal.valueOf(15.5D), Calendar.getInstance());
%  /* ** Numeric literals are used within the assertion statement ** */
%  assertEquals(15, localTime.get(Calendar.HOUR_OF_DAY)) ;
%  assertEquals(30, localTime.get(Calendar.MINUTE)) ;
%}
%\end{lstlisting}

\ifdefined\modoTese
\subsection*{26 -- Manual Intervention (MI)~\cite{meszaros2007xunit}:}\label{smell:mi}
\else
\subsubsection*{26 -- Manual Intervention (MI)~\cite{meszaros2007xunit}:}\label{smell:mi}
\fi
%
\noindent\textit{Description:} Tests that require some form of manual action to run.

\noindent\textit{Impact:} Such tests are likely to be run less frequently due to the effort they require. As such, this smell makes tests less effective.

%\noindent\textit{Detection Strategy:} A test is either not self-contained or does not have assertions to verify the exercised code~\cite{meszaros2007xunit}.

\ifdefined\modoTese
\subsection*{27 -- Mixed Selectors (MS)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:ms}
\else
\subsubsection*{27 -- Mixed Selectors (MS)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:ms}
\fi
%
\noindent\textit{Description:} A class contains both production methods and test cases.

\noindent\textit{Impact:} This test smell negatively impacts understandability and maintainability.

%\noindent\textit{Detection Strategy:} A class contains both test and production code~\cite{reichhart2007rule}.

\ifdefined\modoTese
\subsection*{28 -- Mystery Guest (MG)~\cite{bavota2015test, grano2019scented, meszaros2007xunit, van2001refactoring, virginio2019influence}:}\label{smell:mg}
\else
\subsubsection*{28 -- Mystery Guest (MG)~\cite{bavota2015test, grano2019scented, meszaros2007xunit, van2001refactoring, virginio2019influence}:}\label{smell:mg}
\fi
%
\noindent\textit{Description:} Tests use external resources (such as files or databases); hence, they are not self-contained.

\noindent\textit{Impact:} Harder to understand and maintain due to the lack of available information. The usage of external resources also introduces hidden dependencies.

%(changing the resources may cause tests to fail).

%\noindent\textit{Detection Strategy:} Check if a test case contains instances of file/database classes~\cite{tsDetectWebsite}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{tsDetectWebsite}.,label=exampleMysteryGuest]
%\begin{lstlisting}[language=Java,caption=Example --- Mystery Guest~\protect\cite{tsDetectWebsite}.,label=exampleMysteryGuest]
% @Test
%public void testPersistence() throws Exception {
%  File tempFile = File.createTempFile("systemstate-", ".txt");
%  try {
%    SystemState a = new SystemState(then, 27, false, bootTimestamp);
%    a.addInstalledApp("a.b.c", "ABC", "1.2.3");
%
%    a.writeToFile(tempFile);
%    SystemState b = SystemState.readFromFile(tempFile);
%
%    assertEquals(a, b);
%  } finally {
%    //noinspection ConstantConditions
%    if (tempFile != null) {
%      //noinspection ResultOfMethodCallIgnored
%      tempFile.delete();
%    }
%  }
%}
%\end{lstlisting}

\ifdefined\modoTese
\subsection*{29 -- Non-Java Smells (NJS)}\label{smell:njs}
\else
\subsubsection*{29 -- Non-Java Smells (NJS)}\label{smell:njs}
\fi
%
\noindent\textit{Description:} This does not correspond to a specific smell. Instead, it represents a set of simple test smells associated with concepts unrelated to Java. We have decided to combine the following smells into this category:
%
\begin{itemize}

  \item \textbf{Empty Method Category~\cite{reichhart2007rule}:} Test case with an empty method category.
  
  \item \textbf{Empty Test-Method Category~\cite{reichhart2007rule}:} Test case with an empty test method category.
  
  \item \textbf{TTCN-3 Smells~\cite{baker2006trex}:} Set of test smells specific to TTCN-3 test suites.
  
  \item \textbf{Unclassified Method Category~\cite{reichhart2007rule}:} Test cases not organized by a method-category.
  
\end{itemize}

\ifdefined\modoTese
\subsection*{30 -- Obscure In-line Setup (OISS)~\cite{aljedaani2021test, greiler2013automated}:}\label{smell:oiss}
\else
\subsubsection*{30 -- Obscure In-line Setup (OISS)~\cite{aljedaani2021test, greiler2013automated}:}\label{smell:oiss}
\fi
%Test cases that contain too much setup functionality. This smell makes the test cases difficult to understand and maintain (the in-line setup should only contain what is essential to understand the test). The acceptable amount of setup information in a test case is dependent on the characteristics of the respective test.

%A test case contains too much setup functionality (an in-line should only have what is needed to understand the test). This smell makes test cases harder to understand and maintain. The acceptable amount of setup information in a test case is dependent on the characteristics of the respective test. There exists a variant of this smell: Max Instance Variables (MIV)~\cite{reichhart2007rule} - Overly large fixture.

\noindent\textit{Description:} A test case contains an excessive amount of setup functionality (an in-line setup should only have what is required to understand the test).

\noindent\textit{Impact:} Harder to understand and maintain.

\noindent\textit{Variants:} Max Instance Variables (MIV)~\cite{reichhart2007rule} --- Overly large fixture.

%\noindent\textit{Detection:} Check whether the number of declared variables in a test case exceeds a given limit~\cite{greiler2013automated}.

\noindent\textit{Note:} The acceptable amount of setup information in a test case is dependent on the characteristics of the respective test and the production class.

\ifdefined\modoTese
\subsection*{31 -- Overcommented Test (OCT)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:oct}
\else
\subsubsection*{31 -- Overcommented Test (OCT)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:oct}
\fi
%
\noindent\textit{Description:} A test contains too many comments.

\noindent\textit{Impact:} This smell can make the tests harder to understand (which is the opposite of what comments should do).

%\noindent\textit{Detection Strategy:} Count the total number of comments in a test case~\cite{reichhart2007rule}.

%\noindent\textit{Note:} The acceptable number of comments in a test case is dependent on the characteristics of the respective test.

\ifdefined\modoTese
\subsection*{32 -- Overreferencing (OF)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:of}
\else
\subsubsection*{32 -- Overreferencing (OF)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:of}
\fi
%
\noindent\textit{Description:} A test case that references classes an excessive number of times.

\noindent\textit{Impact:} This test smell makes the tests more difficult to understand and maintain.

%\noindent\textit{Detection:} Verify if the number of referenced classes in a test case exceeds a certain threshold~\cite{reichhart2007rule}.

\noindent\textit{Note:} The acceptable amount of referenced classes in a test case is dependent on the characteristics of the respective test and the production class.

\ifdefined\modoTese
\subsection*{33 -- Proper Organization (PO)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:po}
\else
\subsubsection*{33 -- Proper Organization (PO)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:po}
\fi
%
\noindent\textit{Description:} Poorly organized test cases that do not respect testing conventions.

\noindent\textit{Impact:} Harder to understand and maintain.

%\noindent\textit{Detection Strategy:} A test case is not organized according to a specific set of rules~\cite{reichhart2007rule}.

\ifdefined\modoTese
\subsection*{34 -- Redundant Assertion (RA)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite, virginio2019influence}:}\label{smell:ra}
\else
\subsubsection*{34 -- Redundant Assertion (RA)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite, virginio2019influence}:}\label{smell:ra}
\fi
%
\noindent\textit{Description:} Test cases have assertions that are permanently true/false (e.g., assertions with equal values for the actual and expected parameters).

\noindent\textit{Impact:} This smell makes the tests less effective (it can give a false sense of security).

%\noindent\textit{Detection:} A test case has at least one assertion with the same values for the actual and expected parameters~\cite{tsDetectWebsite}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{peruma2019distribution}.,label=exampleRedundantAssertion]
\begin{lstlisting}[language=Java,caption=Example of a test case with the ``Redundant Assertion'' test smell~\protect\cite{peruma2019distribution}.,label=exampleRedundantAssertion]
@Test
public void testTrue() {
  /* ** Assert statement will always return true ** */
  assertEquals(true, true) ;
}
\end{lstlisting}

\ifdefined\modoTese
\subsection*{35 -- Redundant Print (RP)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite}:}\label{smell:rp}
\else
\subsubsection*{35 -- Redundant Print (RP)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite}:}\label{smell:rp}
\fi
%
\noindent\textit{Description:} Test cases have (unnecessary) print statements.

\noindent\textit{Impact:} May hinder test effectiveness (print statements consume both time and resources).

\noindent\textit{Variants:} Transcripting Test (TT)~\cite{reichhart2007rule} --- corresponds to printing/logging to the console.

\ifdefined\modoTese
\subsection*{36 -- Resource Optimism (RO)~\cite{grano2019scented, tsDetectWebsite, spadini2018relation, spadini2020investigating, van2001refactoring}:}\label{smell:ro}
\else
\subsubsection*{36 -- Resource Optimism (RO)~\cite{grano2019scented, tsDetectWebsite, spadini2018relation, spadini2020investigating, van2001refactoring}:}\label{smell:ro}
\fi
%
\noindent\textit{Description:} Test cases that make optimistic assumptions about the existence/state of external resources.

\noindent\textit{Impact:} This smell can lead to non-deterministic test results.

%(dependent on the state of the resources).

%\noindent\textit{Detection Strategy:} Check if a test case uses an instance of a File class without first calling one of the following methods: \emph{exists()}; \emph{notExists()}; \emph{isFile()}~\cite{tsDetectWebsite}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{tsDetectWebsite}.,label=exampleResourceOptimism]
%\begin{lstlisting}[language=Java,caption=Example --- Resource Optimism~\protect\cite{tsDetectWebsite}.,label=exampleResourceOptimism]
%@Test
%public void saveImage_noImageFile_ko() throws IOException {
%  File outputFile = File.createTempFile("prefix", "png", new File("/tmp"));
%  ProductImage image = new ProductImage("01010101010101", ProductImageField.FRONT, outputFile);
%  Response response = serviceWrite.saveImage(image.getCode(), image.getField(), image.getImguploadFront(), image.getImguploadIngredients(), image.getImguploadNutrition()).execute();
%  assertTrue(response.isSuccess());
%  assertThatJson(response.body()).node("status")
%    .isEqualTo("status not ok");
%}
%\end{lstlisting}

\ifdefined\modoTese
\subsection*{37 -- Returning Assertion (ReA)~\cite{aljedaani2021test}:}\label{smell:rea}
\else
\subsubsection*{37 -- Returning Assertion (ReA)~\cite{aljedaani2021test}:}\label{smell:rea}
\fi
%
\noindent\textit{Description:} A test case contains assertions and also returns a value.

\noindent\textit{Impact:} This smell affects maintainability and comprehensibility.

\ifdefined\modoTese
\subsection*{38 -- Rotten Green Tests (RGT)~\cite{aljedaani2021test, aranega2021rotten, delplanque2019rotten}:}\label{smell:rgt}
\else
\subsubsection*{38 -- Rotten Green Tests (RGT)~\cite{aljedaani2021test, aranega2021rotten, delplanque2019rotten}:}\label{smell:rgt}
\fi
%
\noindent\textit{Description:} Test cases affected by this smell can pass without executing at least one assertion, thus giving a false sense of security.

\noindent\textit{Impact:} Less effective.

\noindent\textit{Variants:} Early Returning Test (ERT) --- the test case does not execute certain assertions because it returns a value too early.

%Specific variant of this smell that describes one of the concrete situations in which assertions are not executed: 

%Test cases have assertions that are not executed. 

%\noindent\textit{Detection:} Check whether it is possible for a test to pass without executing at least one assertion~\cite{aranega2021rotten}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{aranega2021rotten}.,label=exampleRottenGreenTests]
\begin{lstlisting}[language=Java,caption=Example of a test case with the ``Rotten Green Tests'' test smell~\protect\cite{aranega2021rotten}.,label=exampleRottenGreenTests]
@Test
public void testLoggerContainsLogEntry(){
  Logger logger = new Logger();
  logger.log("log1");
  logger.log("log2");
  for (LogEntry logEntry : logger.getLogEntries()){
    assertTrue(logger.containsLogEntry(logEntry));
  }
}
\end{lstlisting}

\ifdefined\modoTese
\subsection*{39 -- Sensitive Equality (SE)~\cite{grano2019scented, Panichella2020, tsDetectWebsite, spadini2018relation, van2001refactoring}:}\label{smell:se}
\else
\subsubsection*{39 -- Sensitive Equality (SE)~\cite{grano2019scented, Panichella2020, tsDetectWebsite, spadini2018relation, van2001refactoring}:}\label{smell:se}
\fi
%
\noindent\textit{Description:} A test has assertions that perform equality checks using the \emph{toString} method.

\noindent\textit{Impact:} Test cases become dependent on (irrelevant) details of the String used in the comparison. Moreover, if the \emph{toString} method for an object changes, the test starts failing.

%Due to this smell, test cases become dependent on the (irrelevant) details of the String used in the comparison. Furthermore, if the \emph{toString} method for an object is changed, the test starts failing.

%\noindent\textit{Detection:} Verify if a test case calls the method \emph{toString()} of a given object~\cite{tsDetectWebsite}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{Panichella2020}.,label=exampleSensitiveEquality]
\begin{lstlisting}[language=Java,caption=Example of a test case with the ``Sensitive Equality'' test smell~\protect\cite{Panichella2020}.,label=exampleSensitiveEquality]
@Test
public void test62() throws Throwable {
  SubstringLabeler.Match substringLabeler_Match0 = new SubstringLabeler.Match();
  String string0 = substringLabeler_Match0.toString();
  assertEquals("Substring: [Atts: ]", string0);
}
\end{lstlisting}

\ifdefined\modoTese
\subsection*{40 -- Sleepy Test (ST)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite}:}\label{smell:st}
\else
\subsubsection*{40 -- Sleepy Test (ST)~\cite{aljedaani2021test, peruma2019distribution, tsDetectWebsite}:}\label{smell:st}
\fi
%
\noindent\textit{Description:} Temporarily stopping the execution of a test case.

\noindent\textit{Impact:} Less effective --- pausing a thread can trigger unexpected results.

%Pausing a thread can trigger unexpected results. As such, this smell makes tests less effective.

\ifdefined\modoTese
\subsection*{41 -- Slow Tests (SloT)~\cite{meszaros2007xunit}:}\label{smell:slot}
\else
\subsubsection*{41 -- Slow Tests (SloT)~\cite{meszaros2007xunit}:}\label{smell:slot}
\fi
%
\noindent\textit{Description:} Tests take a long time to run. This smell can arise as a result of (1) poorly designed test code or (2) the characteristics of the system under test.

%This test smell can occur for one of two reasons: (1) poorly designed test code; (2) the characteristics of the system under test.

\noindent\textit{Impact:} Slow tests are less effective and are likely to be run less frequently.

%\noindent\textit{Detection Strategy:} Measure how long a test takes to run and compare the resulting duration with another duration (e.g., compare the duration of two equivalent test cases). Alternatively, verify whether a test contains code that might make it slow (such as slow components)~\cite{meszaros2007xunit}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{meszaros2007xunit}.,label=exampleSlowTests]
%\begin{lstlisting}[language=Java,caption=Example --- Slow Tests~\protect\cite{meszaros2007xunit}.,label=exampleSlowTests]
% @Test
%public void testHandleOneRequest_Async() throws InterruptedException {
%  // Setup
%  RequestHandlerThread sut = new RequestHandlerThread();
%  sut.start();
%  // Exercise
%  enqueRequest(makeSimpleRequest());
%  // Verify
%  Thread.sleep(TWO_SECONDS);
%  assertEquals(1, sut.getNumberOfRequestsCompleted());
%  assertResponseEquals(makeSimpleResponse(), getResponse());
%}
%\end{lstlisting}

\ifdefined\modoTese
\subsection*{42 -- Teardown Only Test (TOT)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:tot}
\else
\subsubsection*{42 -- Teardown Only Test (TOT)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:tot}
\fi
%
\noindent\textit{Description:} Test suites that only specify teardown.

\noindent\textit{Impact:} This smell affects maintainability.

\ifdefined\modoTese
\subsection*{43 -- Test Code Duplication (TCD)~\cite{aljedaani2021test, bavota2015test, bavota2012empirical, meszaros2007xunit, van2001refactoring}:}\label{smell:tcd}
\else
\subsubsection*{43 -- Test Code Duplication (TCD)~\cite{aljedaani2021test, bavota2015test, bavota2012empirical, meszaros2007xunit, van2001refactoring}:}\label{smell:tcd}
\fi
%
\noindent\textit{Description:} Unwanted duplication in the test code. Test code duplication can be present amongst several tests or within the same test.

\noindent\textit{Impact:} This smell affects maintainability and comprehensibility.

\noindent\textit{Alternative Designations:} Duplicated Code~\cite{breugelmans2008testq}.

%\noindent\textit{Detection Strategy:} Verify if test cases contain repeated groups of similar statements (e.g., utilize the Levenshtein Distance)~\cite{meszaros2007xunit}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{meszaros2007xunit}.,label=exampleTestCodeDuplication]
%\begin{lstlisting}[language=Java,caption=Example --- Test Code Duplication~\protect\cite{meszaros2007xunit}.,label=exampleTestCodeDuplication]
% @Test
%public void testInvoice_addTwoLineItems_sameProduct() {
%  Invoice inv = createAnonInvoice();
%  LineItem expItem1 = new LineItem(inv, product, QUANTITY1);
%  LineItem expItem2 = new LineItem(inv, product, QUANTITY2);
%  // Exercise
%  inv.addItemQuantity(product, QUANTITY1);
%  inv.addItemQuantity(product, QUANTITY2);
%  // Verify
%  List lineItems = inv.getLineItems();
%  assertEquals("number of items", lineItems.size(), 2);
%  // Verify first item
%  LineItem actual = (LineItem)lineItems.get(0);
%  assertEquals(expItem1.getInv(), actual.getInv());
%  assertEquals(expItem1.getProd(), actual.getProd());
%  assertEquals(expItem1.getQuantity(), actual.getQuantity());
%  // Verify second item
%  actual = (LineItem)lineItems.get(1);
%  assertEquals(expItem2.getInv(), actual.getInv());
%  assertEquals(expItem2.getProd(), actual.getProd());
%  assertEquals(expItem2.getQuantity(), actual.getQuantity());
%}
%\end{lstlisting}

\ifdefined\modoTese
\subsection*{44 -- Test Logic in Production (TLP)~\cite{meszaros2007xunit}:}\label{smell:tlp}
\else
\subsubsection*{44 -- Test Logic in Production (TLP)~\cite{meszaros2007xunit}:}\label{smell:tlp}
\fi
%
\noindent\textit{Description:} Production code contains logic that should solely be exercised when testing.

\noindent\textit{Impact:} This smell makes the system under test more complex and fault-prone (serious problems can arise when running the test-specific code in a production environment).

\ifdefined\modoTese
\subsection*{45 -- Test Maverick (TM)~\cite{aljedaani2021test, greiler2013automated}:}\label{smell:tm}
\else
\subsubsection*{45 -- Test Maverick (TM)~\cite{aljedaani2021test, greiler2013automated}:}\label{smell:tm}
\fi
%
\noindent\textit{Description:} A test suite contains test cases independent of the existing implicit setup.

\noindent\textit{Impact:} Run slower (unnecessary extra work). Harder to comprehend and maintain.

\ifdefined\modoTese
\subsection*{46 -- Test Pollution (TP)~\cite{aljedaani2021test, gyori2015reliable}:}\label{smell:tp}
\else
\subsubsection*{46 --Test Pollution (TP)~\cite{aljedaani2021test, gyori2015reliable}:}\label{smell:tp}
\fi
%
\noindent\textit{Description:} Dependent tests that can use (i.e., read/write) shared resources.

\noindent\textit{Impact:} This test smell negatively impacts understandability and maintainability.

%Tests that utilize the same shared resources become dependent on each other. 

%\noindent\textit{Detection Strategy:} Check whether a test case changes the state shared across multiple tests~\cite{gyori2015reliable}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{gyori2015reliable}.,label=exampleTestPollution]
%\begin{lstlisting}[language=Java,caption=Example --- Test Pollution~\protect\cite{gyori2015reliable}.,label=exampleTestPollution]
%public class TestPathData {
%  static Path testDir;
%  ...
%  @BeforeClass
%  public static void initialize() {
%    ...
%    testDir = new Path(System.getProperty("test.build.data", "build/test/data") + "/testPD");
%  }
%  @Test // FT
%  public void testAbsoluteGlob() {
%    PathData[] items = PathData.expandAsGlob(testDir + "/d1/f1*", conf);
%    assertEquals(sortedString(testDir + "/d1/f1", testDir + "/d1/f1.1"), sortedString(items));
%  }
%  ...
%  @Test // PT
%  public void testWithStringAndConfForBuggyPath() {
%    dirString = "file:///tmp";
%    testDir = new Path(dirString);
%    assertEquals("file:/tmp", testDir.toString());
%    ...
%  }
%}
%\end{lstlisting}

\ifdefined\modoTese
\subsection*{47 -- Test Redundancy (TR)~\cite{aljedaani2021test, koochakzadeh2010tecrevis}:}\label{smell:tr}
\else
\subsubsection*{47 -- Test Redundancy (TR)~\cite{aljedaani2021test, koochakzadeh2010tecrevis}:}\label{smell:tr}
\fi
%
\noindent\textit{Description:} One or more test cases can be removed without affecting the fault detection effectiveness of the test suite.

\noindent\textit{Impact:} Redundant test cases only make the test suite harder to understand and maintain.

%\noindent\textit{Detection:} Verify if a test case can be removed from the test suite without decreasing test effectiveness~\cite{koochakzadeh2010tecrevis}.

%\begin{lstlisting}[language=Java,caption=Example - Test Redundancy.,label=exampleTestRedundancy]
\begin{lstlisting}[language=Java,caption=Example of a test suite with the ``Test Redundancy'' test smell.,label=exampleTestRedundancy]  
@Test
public void test00()  throws Throwable  {
  int int0 = Login.getPASSWORDENC();
  assertEquals(2, int0);
}
  
@Test
public void test01()  throws Throwable  {
  int int0 = Login.getPASSWORDENC();
  assertEquals(2, int0);
}
\end{lstlisting}

%\begin{lstlisting}[language=Java,caption=Example --- Test Redundancy.,label=exampleTestRedundancy]
%public class Login_ESTest extends Login_ESTest_scaffolding {
%  @Test
%  public void test00()  throws Throwable  {
%    int int0 = Login.getPASSWORDENC();
%    assertEquals(2, int0);
%  }
%  
%  @Test
%  public void test01()  throws Throwable  {
%    int int0 = Login.getPASSWORDENC();
%    assertEquals(2, int0);
%  }
%}
%\end{lstlisting}

\ifdefined\modoTese
\subsection*{48 -- Test Run War (TRW)~\cite{aljedaani2021test, bavota2015test, bavota2012empirical, meszaros2007xunit, van2001refactoring}:}\label{smell:trw}
\else
\subsubsection*{48 -- Test Run War (TRW)~\cite{aljedaani2021test, bavota2015test, bavota2012empirical, meszaros2007xunit, van2001refactoring}:}\label{smell:trw}
\fi
%
\noindent\textit{Description:} Tests allocate resources (e.g., temporary files) used by various people.

\noindent\textit{Impact:} These tests may fail if different people run them simultaneously.

%\noindent\textit{Detection Strategy:} Check whether a test contains a global shared fixture that can be modified by different tests~\cite{meszaros2007xunit}.

%\noindent\textit{Note:} For example, this smell can arise when testing a class that persists data in a database~\cite{meszaros2007xunit}.

\ifdefined\modoTese
\subsection*{49 -- Test-Class Name (TCN)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:tcn}
\else
\subsubsection*{49 -- Test-Class Name (TCN)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:tcn}
\fi
%
\noindent\textit{Description:} Test suites with non-descriptive names.

\noindent\textit{Impact:} Harder to understand and maintain.

%\noindent\textit{Detection Strategy:} Check whether the name of a test suite follows a specific pattern~\cite{reichhart2007rule}.

%\subsubsection{TTCN-3 Smells (TTCN)~\cite{baker2006trex, aljedaani2021test}}\label{smell:ttcn}
%Set of test smells specific to TTCN-3 test suites.

%\subsubsection{Unclassified Method Category (UMC)~\cite{reichhart2007rule, aljedaani2021test}}\label{smell:umc}
%Test cases that are not organized by a method-category.

\ifdefined\modoTese
\subsection*{50 -- Unknown Test (UT)~\cite{aljedaani2021test, Panichella2020, peruma2019distribution, tsDetectWebsite, virginio2019influence}:}\label{smell:ut}
\else
\subsubsection*{50 -- Unknown Test (UT)~\cite{aljedaani2021test, Panichella2020, peruma2019distribution, tsDetectWebsite, virginio2019influence}:}\label{smell:ut}
\fi
%
\noindent\textit{Description:} Test cases without valid assertions or \emph{@Test(expected)} annotations.

\noindent\textit{Impact:} Less effective because there are no assertions to check whether the results are as expected. Harder to understand and maintain as there are no assertions to examine, thereby making it harder to deduce the purpose of the test cases (this is especially apparent when test cases have non-descriptive names).

\noindent\textit{Alternative Designations:} Assertionless~\cite{breugelmans2008testq} and Assertionless Test~\cite{reichhart2007rule}.

\noindent\textit{Variants:} Under-the-carpet Assertion (UCA)~\cite{reichhart2007rule} --- corresponds to commenting-out assertions in a test case (if only failing assertions are commented-out from the test case, then it is considered an Under-the-carpet failing Assertion (UCFA)~\cite{reichhart2007rule} smell).

%\noindent\textit{Detection:} Check whether a test case does not have at least one assertion or \emph{@Test(expected)} annotation parameter~\cite{tsDetectWebsite}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{peruma2019distribution}.,label=exampleUnknownTest]
\begin{lstlisting}[language=Java,caption=Example of a test case with the ``Unknown Test`` test smell~\protect\cite{peruma2019distribution}.,label=exampleUnknownTest]
@Test
public void hitGetPOICategoriesApi() throws Exception {
  POICategories poiCategories = apiClient.getPOICategories(16);
  for (POICategory category : poiCategories) {
    System.out.println(category.name() + ": " + category);
  }
}

\end{lstlisting}

\ifdefined\modoTese
\subsection*{51 -- Unused Inputs (UI)~\cite{aljedaani2021test, huo2014improving}:}\label{smell:ui}
\else
\subsubsection*{51 -- Unused Inputs (UI)~\cite{aljedaani2021test, huo2014improving}:}\label{smell:ui}
\fi
%
\noindent\textit{Description:} A test case has no assertions to check the particular values that said test case manipulates (the test case is checking too little).

%Assertions do not check values derived from input influenced by a test case (the test case is checking too little).

\noindent\textit{Impact:} Less effective (as it may be unable to reveal faults) and harder to maintain.

%\noindent\textit{Impact:} Such tests are less effective as they may be unable to reveal faults. This test smell can also negatively impact maintainability.

%\noindent\textit{Detection:} Verify if a test case does not contain assertions that check values manipulated by the test~\cite{huo2014improving}.

%\begin{lstlisting}[language=Java,caption=Example taken from~\citet{huo2014improving}.,label=exampleUnusedInputs]
\begin{lstlisting}[language=Java,caption=Example of a test case with the ``Unused Inputs'' test smell~\protect\cite{huo2014improving}.,label=exampleUnusedInputs]
@Test
public void testGetRowKey() {
  DefaultKeyedValues2D d = new DefaultKeyedValues2D();
  
  d.addValue(new Double(1.0), "R1", "C1");
  d.addValue(new Double(1.0), "R2", "C1");
  assertEquals("R1", d.getRowKey(0));
  assertEquals("R2", d.getRowKey(1));
}
\end{lstlisting}

%\subsubsection{Unused Shared-Fixture Variables (USFV)~\cite{reichhart2007rule}}\label{smell:usfv}
%Part of a fixture is never used. This smell affects understandability and makes the tests run slower due to the unnecessary work.

\ifdefined\modoTese
\subsection*{52 -- Unusual Test Order (UTO)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:uto}
\else
\subsubsection*{52 -- Unusual Test Order (UTO)~\cite{aljedaani2021test, reichhart2007rule}:}\label{smell:uto}
\fi
%
\noindent\textit{Description:} A test that directly calls other tests.

\noindent\textit{Impact:} Such tests may manifest erratic behavior (i.e., they are less effective).

%\noindent\textit{Detection Strategy:} A test case which calls another test case~\cite{reichhart2007rule}.

\ifdefined\modoTese
\subsection*{53 -- Vague Header Setup (VHS)~\cite{aljedaani2021test, greiler2013automated}:}\label{smell:vhs}
\else
\subsubsection*{53 -- Vague Header Setup (VHS)~\cite{aljedaani2021test, greiler2013automated}:}\label{smell:vhs}
\fi
%
\noindent\textit{Description:} Fields are initialized in the class header rather than in an implicit setup.

\noindent\textit{Impact:} Harder to understand and maintain.

\ifdefined\modoTese
\subsection*{54 -- Verbose Test (VT)~\cite{breugelmans2008testq, meszaros2007xunit, reichhart2007rule, spadini2020investigating, virginio2019influence}:}\label{smell:vt}
\else
\subsubsection*{54 -- Verbose Test (VT)~\cite{breugelmans2008testq, meszaros2007xunit, reichhart2007rule, spadini2020investigating, virginio2019influence}:}\label{smell:vt}
\fi
%
\noindent\textit{Description:} A test case contains an excessive number of statements (i.e., the test is unnecessarily long). As a result, the test code is neither clean nor simple.

\noindent\textit{Impact:} The excessive number of statements makes the tests harder to understand and maintain. Such tests are also more likely to contain other types of test smells~\cite{grano2019scented}.

\noindent\textit{Alternative Designations:} Complex Test~\cite{meszaros2007xunit}, Long Test~\cite{reichhart2007rule} and Obscure Test~\cite{meszaros2007xunit}.

%\noindent\textit{Detection:} Check whether a test case contains an excessive number of statements~\cite{spadini2020investigating}.

\noindent\textit{Note:} The acceptable number of statements in a test case is dependent on the situation at hand (e.g., 20 statements may be too much in some situations and not enough in others).