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

package morphir.examples.booksandrecords.serviceImpl

import morphir.examples.booksandrecords.API
import morphir.examples.booksandrecords.processor.CmdProcessor.processCmd

import morphir.examples.booksandrecords.repository.DealRepository
import morphir.examples.booksandrecords.service.ServiceStatefulApp
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service

@Service class StatefulApp extends ServiceStatefulApp {
  @Autowired private val dealRepository: DealRepository = null

  override def logic(dealCmd: API.DealCmd) =
    processCmd(dealCmd, dealRepository)

}
