package com.ms.booksandrecords.service

import com.ms.booksandrecords.commands.DealCmd

trait ServiceStatefulApp {
  def logic(dealCmd: DealCmd): DealEvent
}