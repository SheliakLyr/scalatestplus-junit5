/*
 * Copyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatestplus.junit5 {

  import org.scalatest._
  import org.scalatest.events._

  // Put fixture suites in a subpackage, so they won't be discovered by
  // -m org.scalatest.junit when running the test target for this project.
  package helpers {

    import org.junit.jupiter.api.Assertions.assertEquals
    import org.junit.jupiter.api.Test
    import org.junit.jupiter.api.Disabled

    class HappySuite extends JUnitSuite {

      @Test def verifySomething(): Unit = () // Don't do nothin
      //@Test def verifySomething2(): Unit = () // Don't do nothin// '
    }

    class BitterSuite extends JUnitSuite {

      @Test def verifySomething(): Unit = {
        assertEquals(1, 2) // This will fail
      }
    }

    class IgnoredSuite extends JUnitSuite {

      @Disabled @Test def verifySomething(): Unit = {
        assertEquals(1, 2) // This would fail if it were not ignored
      }
    }

    // Used to make sure TestStarting gets fired twice
    class ManySuite extends JUnitSuite {

      @Test def verifySomething(): Unit = ()
      @Test def verifySomethingElse(): Unit = ()
    }

    // -----------------------------------------------------------------------
    // Helpers for execute-method tests in ScalaTestEngineSpec
    // -----------------------------------------------------------------------

    /**
     * A minimal pure ScalaTest suite (no inner JUnit launcher) used to verify
     * that the engine's sequential execution path fires all expected lifecycle
     * events through the reporter chain.
     */
    class SimpleScalaTestSuite extends org.scalatest.funspec.AnyFunSpec {
      it("a simple passing test") { () }
    }

    /** Always-failing suite used to verify sequential execution continues after a failure. */
    class FailingScalaTestSuite extends org.scalatest.funspec.AnyFunSpec {
      it("a test that always fails") {
        fail("intentional failure")
      }
    }

    object AnyFunSuiteTestTag extends org.scalatest.Tag("com.example.AnyFunSuiteTestTag")
    object TaggedAnyFunSuiteState {
      @volatile var testRan: Boolean = false
    }

    class TaggedAnyFunSuite extends org.scalatest.funsuite.AnyFunSuite {
      test("a test with a ScalaTest tag", AnyFunSuiteTestTag) {
        TaggedAnyFunSuiteState.testRan = true
      }
    }

    // -----------------------------------------------------------------------
    // Helpers for parallel-suite-execution tests in ScalaTestEngineSpec
    // -----------------------------------------------------------------------

    import java.util.concurrent.{CyclicBarrier, TimeUnit}

    /**
     * Shared state for the two parallel helper suites below.
     * Call <code>reset()</code> before each use to obtain a fresh barrier.
     */
    object ParallelSuiteHelper {
      @volatile var thread1Name: String = null
      @volatile var thread2Name: String = null
      @volatile var testThread1Name: String = null
      @volatile var testThread2Name: String = null
      @volatile var barrier: CyclicBarrier = new CyclicBarrier(2)

      def reset(): Unit = {
        thread1Name = null
        thread2Name = null
        testThread1Name = null
        testThread2Name = null
        barrier = new CyclicBarrier(2)
      }
    }

    /**
     * Records the executing thread name then waits at a shared barrier.
     * When run together with <code>ParallelHelperSuite2</code> under
     * <code>numThreads >= 2</code> both suites reach the barrier
     * simultaneously, proving concurrent execution.
     */
    class ParallelHelperSuite1 extends org.scalatest.funspec.AnyFunSpec {
      it("a test in parallel suite 1") {
        ParallelSuiteHelper.thread1Name = Thread.currentThread().getName
        ParallelSuiteHelper.barrier.await(5, TimeUnit.SECONDS)
      }
    }

    /** Counterpart of <code>ParallelHelperSuite1</code>. */
    class ParallelHelperSuite2 extends org.scalatest.funspec.AnyFunSpec {
      it("a test in parallel suite 2") {
        ParallelSuiteHelper.thread2Name = Thread.currentThread().getName
        ParallelSuiteHelper.barrier.await(5, TimeUnit.SECONDS)
      }
    }

    class ParallelTestExecutionSuite1 extends org.scalatest.funspec.AnyFunSpec with ParallelTestExecution {
      it("a distributed test in suite 1") {
        ParallelSuiteHelper.testThread1Name = Thread.currentThread().getName
        ParallelSuiteHelper.barrier.await(5, TimeUnit.SECONDS)
      }
    }

    class ParallelTestExecutionSuite2 extends org.scalatest.funspec.AnyFunSpec with ParallelTestExecution {
      it("a distributed test in suite 2") {
        ParallelSuiteHelper.testThread2Name = Thread.currentThread().getName
        ParallelSuiteHelper.barrier.await(5, TimeUnit.SECONDS)
      }
    }

    /** Two distributed tests so the default cached pool can actually overlap them. */
    class ParallelTestExecutionOverlapSuite extends org.scalatest.funspec.AnyFunSpec with ParallelTestExecution {
      it("distributed overlap test 1") {
        ParallelSuiteHelper.testThread1Name = Thread.currentThread().getName
        ParallelSuiteHelper.barrier.await(5, TimeUnit.SECONDS)
      }
      it("distributed overlap test 2") {
        ParallelSuiteHelper.testThread2Name = Thread.currentThread().getName
        ParallelSuiteHelper.barrier.await(5, TimeUnit.SECONDS)
      }
    }

    /** Fires SuiteAborted for the suite under test, with no throwable. */
    class SuiteAbortingSuite extends org.scalatest.funspec.AnyFunSpec {
      override def run(testName: Option[String], args: org.scalatest.Args): org.scalatest.Status = {
        args.reporter(org.scalatest.events.SuiteAborted(
          args.tracker.nextOrdinal(),
          "suite aborted without a throwable",
          suiteName,
          suiteId,
          Some(getClass.getName)
        ))
        org.scalatest.FailedStatus
      }
    }

    /** Fires RunAborted so the engine descriptor must fail without a second finish. */
    class RunAbortingSuite extends org.scalatest.funspec.AnyFunSpec {
      override def run(testName: Option[String], args: org.scalatest.Args): org.scalatest.Status = {
        args.reporter(org.scalatest.events.RunAborted(
          args.tracker.nextOrdinal(),
          "run aborted",
          Some(new RuntimeException("run aborted"))
        ))
        org.scalatest.SucceededStatus
      }
    }
  }

  import helpers._

  class JUnitSuiteSuite extends funsuite.AnyFunSuite {

    class MyReporter extends Reporter {

      var runStartingCount = 0
      var runCompletedCount = 0
      def apply(event: Event): Unit = {
        event match {
          case RunStarting(_, testCount, _, _, _, _, _, _) =>
            runStartingCount += 1
          case event: RunCompleted =>
            runCompletedCount += 1
          case event: TestStarting =>
            testStartingEvent = Some(event)
            testStartingCount += 1
          case event: TestIgnored =>
            testIgnoredEvent = Some(event)
          case event: TestSucceeded =>
            testSucceededEvent = Some(event)
            testSucceededCount += 1
          case event: TestFailed =>
            testFailedEvent = Some(event)
          case _ =>
        }
      }

      var testStartingCount = 0
      var testStartingEvent: Option[TestStarting] = None

      var testSucceededCount = 0
      var testSucceededEvent: Option[TestSucceeded] = None

      var testFailedEvent: Option[TestFailed] = None

      var testIgnoredEvent: Option[TestIgnored] = None
    }

    test("A JUnitSuite with a JUnit 5 Test annotation will cause TestStarting event to be fired") {

      val happy = new HappySuite
      val repA = new MyReporter
      happy.run(None, Args(repA))
      assert(repA.testStartingEvent.isDefined)
      assert(repA.testStartingEvent.get.testName === "verifySomething")
      assert(repA.testStartingEvent.get.suiteName === "HappySuite")
      assert(repA.testStartingEvent.get.suiteClassName.get === "org.scalatestplus.junit5.helpers.HappySuite")
    }

    test("A JUnitSuite with a JUnit 5 Test annotation will cause TestSucceeded to be fired") {

      val happy = new HappySuite
      val repA = new MyReporter
      happy.run(None, Args(repA))
      assert(repA.testSucceededEvent.isDefined)
      assert(repA.testSucceededEvent.get.testName === "verifySomething")
      assert(repA.testSucceededEvent.get.suiteName === "HappySuite")
      assert(repA.testSucceededEvent.get.suiteClassName.get === "org.scalatestplus.junit5.helpers.HappySuite")
    }

    test("A JUnitSuite with a JUnit 5 Test annotation on a bad test will cause testFailed to be invoked") {

      val bitter = new BitterSuite
      val repA = new MyReporter
      bitter.run(None, Args(repA))
      assert(repA.testFailedEvent.isDefined)
      assert(repA.testFailedEvent.get.testName === "verifySomething")
      assert(repA.testFailedEvent.get.suiteName === "BitterSuite")
      assert(repA.testFailedEvent.get.suiteClassName.get === "org.scalatestplus.junit5.helpers.BitterSuite")
      assert(repA.testSucceededCount === 0)
    }

    test("A JUnitSuite with JUnit 5 Disabled and Test annotations will cause TestIgnored to be fired") {

      val ignored = new IgnoredSuite
      val repA = new MyReporter
      ignored.run(None, Args(repA))
      assert(repA.testIgnoredEvent.isDefined)
      assert(repA.testIgnoredEvent.get.testName === "verifySomething")
      assert(repA.testIgnoredEvent.get.suiteName === "IgnoredSuite")
      assert(repA.testIgnoredEvent.get.suiteClassName.get === "org.scalatestplus.junit5.helpers.IgnoredSuite")
    }

    test("A JUnitSuite with two JUnit 5 Test annotations will cause TestStarting and TestSucceeded events to be fired twice each") {

      val many = new ManySuite
      val repA = new MyReporter
      many.run(None, Args(repA))

      assert(repA.testStartingEvent.isDefined)
      assert(repA.testStartingEvent.get.testName startsWith "verifySomething")
      assert(repA.testStartingEvent.get.suiteName === "ManySuite")
      assert(repA.testStartingEvent.get.suiteClassName.get === "org.scalatestplus.junit5.helpers.ManySuite")
      assert(repA.testStartingCount === 2)

      assert(repA.testSucceededEvent.isDefined)
      assert(repA.testSucceededEvent.get.testName startsWith "verifySomething")
      assert(repA.testSucceededEvent.get.suiteName === "ManySuite")
      assert(repA.testSucceededEvent.get.suiteClassName.get === "org.scalatestplus.junit5.helpers.ManySuite")
      assert(repA.testSucceededCount === 2)
    }

    test("A JUnitSuite with a JUnit 5 Test annotation will not cause runStarting to be invoked") {

      val happy = new HappySuite
      val repA = new MyReporter
      happy.run(None, Args(repA))
      assert(repA.runStartingCount === 0)
    }

    test("A JUnitSuite with a JUnit 5 Test annotation will not cause runCompleted to be invoked") {

      val happy = new HappySuite
      val repA = new MyReporter
      happy.run(None, Args(repA))
      assert(repA.runCompletedCount === 0)
    }
  }
}
