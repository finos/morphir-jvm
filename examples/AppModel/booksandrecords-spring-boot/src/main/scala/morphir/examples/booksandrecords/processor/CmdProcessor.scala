/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/


package morphir.examples.booksandrecords.processor

import morphir.examples.booksandrecords.API.{CloseDeal, DealCmd, DealEvent, OpenDeal}
import morphir.examples.booksandrecords.Logic._
import morphir.examples.booksandrecords.repository.DealRepository

object CmdProcessor {
  def processCmd(cmd: DealCmd, dealRepository: DealRepository): DealEvent = {
    cmd match {
      case cmd: OpenDeal => openDeal(cmd, dealRepository)
      case cmd: CloseDeal => closeDeal(cmd, dealRepository)
    }
  }
}