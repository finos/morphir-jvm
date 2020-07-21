package com.ms.booksandrecords.processor.factory

import com.ms.booksandrecords.commands.DealCmd

trait AbstractCmdProcessorFactory[A] {
  def create(cmd: DealCmd): A
}