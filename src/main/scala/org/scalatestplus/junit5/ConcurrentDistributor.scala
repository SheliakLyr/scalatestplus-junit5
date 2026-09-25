/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatestplus.junit5

import org.scalatest._
import org.scalactic.Requirements._
import java.util.concurrent.ExecutorService
import scala.util.{Failure, Success}

/**
 * This Distributor can be used by multiple threads.
 *
 * @author Bill Venners
 */
private[junit5] class ConcurrentDistributor(execSvc: ExecutorService) extends Distributor {
 
  def apply(suite: Suite, args: Args): Status = {
    requireNonNull(suite, args)
    val status = new StatefulStatus
    val suiteRunner = new Runnable {
      override def run(): Unit = {
        try {
          // Do not mark the distributed status complete until the Status returned
          // by the suite has completed. This is important for ParallelTestExecution:
          // suite.run returns before its distributed tests finish.
          val runStatus = suite.run(None, args)
          runStatus.whenCompleted {
            case Success(succeeded) =>
              if (!succeeded)
                status.setFailed()
              status.setCompleted()
            case Failure(t) =>
              status.setFailedWith(t)
              status.setCompleted()
          }
        }
        catch {
          case t: Throwable =>
            status.setFailedWith(t)
            status.setCompleted()
        }
      }
    }
    execSvc.submit(suiteRunner)
    status
  }

  def poll() = None
}
