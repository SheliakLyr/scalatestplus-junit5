package org.scalatestplus.junit5

import org.junit.platform.engine.{ConfigurationParameters, EngineExecutionListener, ExecutionRequest, TestDescriptor, TestExecutionResult}
import org.junit.platform.engine.UniqueId
import org.junit.platform.engine.discovery.ClasspathRootSelector
import org.junit.platform.engine.discovery.DiscoverySelectors.{selectClass, selectClasspathRoots, selectPackage}
import org.junit.platform.launcher.core.LauncherDiscoveryRequestBuilder.request
import org.scalatest.{BeforeAndAfterAll, funspec}
import org.scalatestplus.junit5.helpers.{FailingScalaTestSuite, HappySuite, ParallelHelperSuite1, ParallelHelperSuite2, ParallelSuiteHelper, ParallelTestExecutionOverlapSuite, ParallelTestExecutionSuite1, ParallelTestExecutionSuite2, RunAbortingSuite, SimpleScalaTestSuite, SuiteAbortingSuite, TaggedAnyFunSuite, TaggedAnyFunSuiteState}

import java.nio.file.{Files, Paths}
import java.util.Optional
import java.util.concurrent.CopyOnWriteArrayList
import scala.collection.JavaConverters._

class ScalaTestEngineSpec extends funspec.AnyFunSpec with BeforeAndAfterAll {
  val engine = new ScalaTestEngine
  var scalaTestEngineProperty: Option[String] = None

  /** Minimal no-op ConfigurationParameters for constructing ExecutionRequests in tests. */
  private val emptyConfig: ConfigurationParameters = new ConfigurationParameters {
    def get(key: String): Optional[String] = Optional.empty()
    def getBoolean(key: String): Optional[java.lang.Boolean] = Optional.empty()
    def size(): Int = 0
    def keySet(): java.util.Set[String] = java.util.Collections.emptySet()
  }

  /** Captures executionFinished results for assertion. */
  private def capturingListener(results: CopyOnWriteArrayList[TestExecutionResult]): EngineExecutionListener =
    capturingListener(results, None)

  /** Captures executionFinished pairs when the assertion needs the descriptor, not just the status. */
  private def capturingListener(
    results: CopyOnWriteArrayList[TestExecutionResult],
    events: Option[CopyOnWriteArrayList[(TestDescriptor, TestExecutionResult)]]
  ): EngineExecutionListener =
    new EngineExecutionListener {
      override def executionStarted(td: TestDescriptor): Unit = ()
      override def executionFinished(td: TestDescriptor, result: TestExecutionResult): Unit = {
        results.add(result)
        events.foreach(_.add((td, result)))
      }
      override def executionSkipped(td: TestDescriptor, reason: String): Unit = ()
    }

  override def beforeAll(): Unit = {
    scalaTestEngineProperty = Option(System.clearProperty("org.scalatestplus.junit5.ScalaTestEngine.disabled"))
  }

  override def afterAll(): Unit = {
    scalaTestEngineProperty.foreach(System.setProperty("org.scalatestplus.junit5.ScalaTestEngine.disabled", _))
  }

  describe("ScalaTestEngine") {
    describe("discover method") {
      it("should discover suites on classpath") {
        val classPathRoot = classOf[ScalaTestEngineSpec].getProtectionDomain.getCodeSource.getLocation
        val discoveryRequest = request.selectors(
          selectClasspathRoots(java.util.Collections.singleton(Paths.get(classPathRoot.toURI)))
        ).build()
        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
        assert(engineDescriptor.getChildren.asScala.exists(td => td.asInstanceOf[ScalaTestClassDescriptor].suiteClass == classOf[HappySuite]))
      }

      it("should return unresolved for classpath without any tests") {
        val emptyPath = Files.createTempDirectory(null)
        val discoveryRequest = request.selectors(
          selectClasspathRoots(java.util.Collections.singleton(emptyPath))
        ).build()

        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
        assert(engineDescriptor.getChildren.asScala.isEmpty)
      }

      it("should discover suites in package") {
        val discoveryRequest = request.selectors(
          selectPackage("org.scalatestplus.junit5.helpers")
        ).build()

        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
        assert(engineDescriptor.getChildren.asScala.exists(td => td.asInstanceOf[ScalaTestClassDescriptor].suiteClass == classOf[HappySuite]))
      }

      it("should return unresolved for package without any tests") {
        val discoveryRequest = request.selectors(
          selectPackage("org.scalatestplus.junit5.nonexistant")
        ).build()

        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
        assert(engineDescriptor.getChildren.asScala.isEmpty)
      }
    }

    describe("execute method") {
      it("should run suites sequentially by default (numThreads=1)") {
        // SimpleScalaTestSuite is a pure AnyFunSpec (no inner JUnit launcher), so
        // all three lifecycle events flow cleanly: test → suite → engine.
        val discoveryRequest = request.selectors(selectClass(classOf[SimpleScalaTestSuite])).build()
        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))

        val results = new CopyOnWriteArrayList[TestExecutionResult]()
        val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
        engine.execute(execRequest)

        // Results: test executionFinished + suite executionFinished + engine executionFinished → 3 entries
        assert(results.size() == 3, s"Expected 3 results (test+suite+engine) but got: $results")
        assert(results.asScala.forall(_.getStatus == TestExecutionResult.Status.SUCCESSFUL),
          s"Unexpected failures: ${results.asScala.filter(_.getStatus != TestExecutionResult.Status.SUCCESSFUL)}")
      }

      it("should continue running remaining suites in sequential mode when one fails") {
        // FailingScalaTestSuite always fails; SimpleScalaTestSuite always passes.
        // Both must execute even when the first suite fails (Bug 2 regression guard).
        val discoveryRequest = request.selectors(
          selectClass(classOf[FailingScalaTestSuite]),
          selectClass(classOf[SimpleScalaTestSuite])
        ).build()
        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))

        val results = new CopyOnWriteArrayList[TestExecutionResult]()
        val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
        // execute() must not throw; suite failures are reported through the listener.
        engine.execute(execRequest)

        val statuses = results.asScala.map(_.getStatus).toList
        // FAILED proves FailingScalaTestSuite's test ran. The suite and engine
        // descriptors stay SUCCESSFUL: JUnit does not aggregate child failures.
        assert(statuses.exists(_ == TestExecutionResult.Status.FAILED),
          s"Expected at least one FAILED result but got: $statuses")
        // SUCCESSFUL proves SimpleScalaTestSuite also ran (was not skipped after the failure).
        assert(statuses.exists(_ == TestExecutionResult.Status.SUCCESSFUL),
          s"Expected at least one SUCCESSFUL result but got: $statuses")
      }

      it("should report SuiteAborted on the class descriptor and fail the engine descriptor") {
        val discoveryRequest = request.selectors(selectClass(classOf[SuiteAbortingSuite])).build()
        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
        val classDescriptor = engineDescriptor.getChildren.asScala.head

        val results = new CopyOnWriteArrayList[TestExecutionResult]()
        val events = new CopyOnWriteArrayList[(TestDescriptor, TestExecutionResult)]()
        val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results, Some(events)), emptyConfig)
        engine.execute(execRequest)

        val classFinishes = events.asScala.filter(_._1 == classDescriptor)
        assert(classFinishes.size == 1, s"Expected one class finish but got: $classFinishes")
        assert(classFinishes.head._2.getStatus == TestExecutionResult.Status.ABORTED,
          s"SuiteAborted must not be overwritten, got: ${classFinishes.head._2.getStatus}")
        val engineFinishes = events.asScala.filter(_._1 == engineDescriptor)
        assert(engineFinishes.size == 1, s"Expected one engine finish but got: $engineFinishes")
        assert(engineFinishes.head._2.getStatus == TestExecutionResult.Status.FAILED,
          s"A suite abort must fail the engine descriptor, got: ${engineFinishes.head._2.getStatus}")
      }

      it("should fail the engine descriptor on RunAborted without finishing it twice") {
        val discoveryRequest = request.selectors(selectClass(classOf[RunAbortingSuite])).build()
        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))

        val events = new CopyOnWriteArrayList[(TestDescriptor, TestExecutionResult)]()
        val execRequest = ExecutionRequest.create(
          engineDescriptor,
          capturingListener(new CopyOnWriteArrayList[TestExecutionResult](), Some(events)),
          emptyConfig
        )
        engine.execute(execRequest)

        val engineFinishes = events.asScala.filter(_._1 == engineDescriptor)
        assert(engineFinishes.size == 1, s"RunAborted must not double-finish the engine, got: $engineFinishes")
        assert(engineFinishes.head._2.getStatus == TestExecutionResult.Status.FAILED,
          s"RunAborted must fail the engine descriptor, got: ${engineFinishes.head._2.getStatus}")
      }

      it("should distribute ParallelTestExecution tests when numThreads is unset") {
        // The historical default is a cached pool. A fixed pool of 1 would leave
        // the second distributed test blocked on the barrier.
        ParallelSuiteHelper.reset()
        System.clearProperty("org.scalatestplus.junit5.numThreads")
        val discoveryRequest = request.selectors(selectClass(classOf[ParallelTestExecutionOverlapSuite])).build()
        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
        val results = new CopyOnWriteArrayList[TestExecutionResult]()
        val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
        engine.execute(execRequest)

        assert(ParallelSuiteHelper.testThread1Name != null, "distributed test 1 did not run")
        assert(ParallelSuiteHelper.testThread2Name != null, "distributed test 2 did not run")
        assert(ParallelSuiteHelper.testThread1Name.startsWith("ScalaTest-"),
          s"Expected the default ParallelTestExecution pool, got: ${ParallelSuiteHelper.testThread1Name}")
        assert(!ParallelSuiteHelper.testThread1Name.startsWith("ScalaTest-Suite-"),
          s"Default ParallelTestExecution must not use the suite pool, got: ${ParallelSuiteHelper.testThread1Name}")
        assert(ParallelSuiteHelper.testThread1Name != ParallelSuiteHelper.testThread2Name,
          "Default numThreads serialized ParallelTestExecution tests")
        assert(results.asScala.forall(_.getStatus == TestExecutionResult.Status.SUCCESSFUL),
          s"Unexpected failures: ${results.asScala.filter(_.getStatus != TestExecutionResult.Status.SUCCESSFUL)}")
      }

      it("should not run a suite when all of its discovered tests were filtered out") {
        TaggedAnyFunSuiteState.testRan = false
        val discoveryRequest = request.selectors(selectClass(classOf[TaggedAnyFunSuite])).build()
        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
        val classDescriptor = engineDescriptor.getChildren.asScala.collectFirst {
          case descriptor: ScalaTestClassDescriptor => descriptor
        }.get

        // Simulate JUnit Platform's post-discovery excludeTags filter removing
        // the suite's only tagged test while retaining the untagged container.
        classDescriptor.getChildren.asScala.foreach(classDescriptor.removeChild)

        val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(new CopyOnWriteArrayList[TestExecutionResult]()), emptyConfig)
        engine.execute(execRequest)

        assert(!TaggedAnyFunSuiteState.testRan,
          "A test removed by post-discovery filtering was executed again")
      }

      it("should run suites in parallel when numThreads=2") {
        // ParallelHelperSuite1 and ParallelHelperSuite2 each record their thread
        // name then await a shared CyclicBarrier(2).  With numThreads=2 both
        // suites run on dedicated "ScalaTest-Suite-N" pool threads concurrently so
        // the barrier trips and both tests complete.  Sequential execution would
        // cause the first suite to time out waiting at the barrier (5 s) → failure.
        ParallelSuiteHelper.reset()
        System.setProperty("org.scalatestplus.junit5.numThreads", "2")
        try {
          val discoveryRequest = request.selectors(
            selectClass(classOf[ParallelHelperSuite1]),
            selectClass(classOf[ParallelHelperSuite2])
          ).build()
          val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))

          val results = new CopyOnWriteArrayList[TestExecutionResult]()
          val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
          engine.execute(execRequest)

          // Both suite tests completed → barrier was released → suites ran in parallel
          assert(ParallelSuiteHelper.thread1Name != null, "ParallelHelperSuite1 did not record a thread name")
          assert(ParallelSuiteHelper.thread2Name != null, "ParallelHelperSuite2 did not record a thread name")
          // Both suites ran in dedicated suite pool threads
          assert(ParallelSuiteHelper.thread1Name.startsWith("ScalaTest-Suite-"),
            s"Expected suite-pool thread name but got: ${ParallelSuiteHelper.thread1Name}")
          assert(ParallelSuiteHelper.thread2Name.startsWith("ScalaTest-Suite-"),
            s"Expected suite-pool thread name but got: ${ParallelSuiteHelper.thread2Name}")
          // The two suites ran on different threads
          assert(ParallelSuiteHelper.thread1Name != ParallelSuiteHelper.thread2Name,
            "Both suites ran on the same thread – they were not truly parallel")
          // All results (engine + 2 suites + 2 tests = 5) must be SUCCESSFUL
          assert(results.asScala.forall(_.getStatus == TestExecutionResult.Status.SUCCESSFUL),
            s"Unexpected failures: ${results.asScala.filter(_.getStatus != TestExecutionResult.Status.SUCCESSFUL)}")
        } finally {
          System.clearProperty("org.scalatestplus.junit5.numThreads")
        }
      }

      it("should run suites in parallel when numThreads=0 (auto-detect)") {
        ParallelSuiteHelper.reset()
        System.setProperty("org.scalatestplus.junit5.numThreads", "0")
        try {
          val discoveryRequest = request.selectors(
            selectClass(classOf[ParallelHelperSuite1]),
            selectClass(classOf[ParallelHelperSuite2])
          ).build()
          val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))

          val results = new CopyOnWriteArrayList[TestExecutionResult]()
          val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
          engine.execute(execRequest)

          assert(ParallelSuiteHelper.thread1Name != null, "ParallelHelperSuite1 did not record a thread name")
          assert(ParallelSuiteHelper.thread2Name != null, "ParallelHelperSuite2 did not record a thread name")
          assert(ParallelSuiteHelper.thread1Name.startsWith("ScalaTest-Suite-"),
            s"Expected suite-pool thread name but got: ${ParallelSuiteHelper.thread1Name}")
          assert(ParallelSuiteHelper.thread2Name.startsWith("ScalaTest-Suite-"),
            s"Expected suite-pool thread name but got: ${ParallelSuiteHelper.thread2Name}")
          assert(ParallelSuiteHelper.thread1Name != ParallelSuiteHelper.thread2Name,
            "Both suites ran on the same thread – they were not truly parallel")
          assert(results.asScala.forall(_.getStatus == TestExecutionResult.Status.SUCCESSFUL),
            s"Unexpected failures: ${results.asScala.filter(_.getStatus != TestExecutionResult.Status.SUCCESSFUL)}")
        } finally {
          System.clearProperty("org.scalatestplus.junit5.numThreads")
        }
      }

      it("should share the suite pool with ParallelTestExecution tests") {
        // The two top-level suite tasks submit their distributed tests to the
        // same run-scoped executor and return immediately. If each suite had a
        // private pool, the test threads would use the old ScalaTest-* prefix.
        ParallelSuiteHelper.reset()
        System.setProperty("org.scalatestplus.junit5.numThreads", "2")
        try {
          val discoveryRequest = request.selectors(
            selectClass(classOf[ParallelTestExecutionSuite1]),
            selectClass(classOf[ParallelTestExecutionSuite2])
          ).build()
          val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))

          val results = new CopyOnWriteArrayList[TestExecutionResult]()
          val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
          engine.execute(execRequest)

          assert(ParallelSuiteHelper.testThread1Name != null,
            "ParallelTestExecutionSuite1 did not record a test thread name")
          assert(ParallelSuiteHelper.testThread2Name != null,
            "ParallelTestExecutionSuite2 did not record a test thread name")
          assert(ParallelSuiteHelper.testThread1Name.startsWith("ScalaTest-Suite-"),
            s"Expected shared suite-pool thread name but got: ${ParallelSuiteHelper.testThread1Name}")
          assert(ParallelSuiteHelper.testThread2Name.startsWith("ScalaTest-Suite-"),
            s"Expected shared suite-pool thread name but got: ${ParallelSuiteHelper.testThread2Name}")
          assert(ParallelSuiteHelper.testThread1Name != ParallelSuiteHelper.testThread2Name,
            "Distributed tests did not run concurrently on distinct shared-pool threads")
          assert(results.asScala.forall(_.getStatus == TestExecutionResult.Status.SUCCESSFUL),
            s"Unexpected failures: ${results.asScala.filter(_.getStatus != TestExecutionResult.Status.SUCCESSFUL)}")
        } finally {
          System.clearProperty("org.scalatestplus.junit5.numThreads")
        }
      }

      it("should throw RuntimeException for invalid numThreads value") {
        System.setProperty("org.scalatestplus.junit5.numThreads", "notANumber")
        try {
          val discoveryRequest = request.selectors(selectClass(classOf[HappySuite])).build()
          val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
          val results = new CopyOnWriteArrayList[TestExecutionResult]()
          val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
          intercept[RuntimeException] {
            engine.execute(execRequest)
          }
          // executionFinished must be called exactly once even when execute() throws,
          // satisfying the JUnit Platform lifecycle contract (Bug 1 fix verification).
          assert(results.size() == 1, s"Expected executionFinished to be called once but got: ${results.size()}")
          assert(results.get(0).getStatus == TestExecutionResult.Status.FAILED,
            s"Expected FAILED engine result but got: ${results.get(0).getStatus}")
        } finally {
          System.clearProperty("org.scalatestplus.junit5.numThreads")
        }
      }

      it("should throw RuntimeException for negative numThreads value") {
        System.setProperty("org.scalatestplus.junit5.numThreads", "-1")
        try {
          val discoveryRequest = request.selectors(selectClass(classOf[HappySuite])).build()
          val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
          val results = new CopyOnWriteArrayList[TestExecutionResult]()
          val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
          intercept[RuntimeException] {
            engine.execute(execRequest)
          }
          // Negative values are invalid (Bug 3 fix); lifecycle contract still respected.
          assert(results.size() == 1, s"Expected executionFinished to be called once but got: ${results.size()}")
          assert(results.get(0).getStatus == TestExecutionResult.Status.FAILED,
            s"Expected FAILED engine result but got: ${results.get(0).getStatus}")
        } finally {
          System.clearProperty("org.scalatestplus.junit5.numThreads")
        }
      }
    }
  }
}
