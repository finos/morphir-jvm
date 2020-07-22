package morphir.examples.booksandrecords

import com.ms.booksandrecords.API.*;
import com.ms.booksandrecords.model.Deal;
import org.springframework.stereotype.Component;

@Component
object Logic {
    def openDeal(command: OpenDeal, dealRepository: DealRepository) : DealEvent = {
        for {
            deal <- dealRepository.findById (command.dealId)
        } yield {
            DuplicateDeal(deal.id)
        } getOrElse {
            if (command.price < 0)
                InvalidPrice(command.dealId, command.price);
            else if (command.quantity < 0)
                InvalidQuantity(command.dealId, command.quantity);
            else
                DealOpened(command.dealId, command.productId, command.price, command.quantity);
        }
    }

    def closeDeal(command: CloseDeal, dealRepository: DealRepository) : DealEvent = {
        for {
            deal <- dealRepository.findById (comand.dealId)
        } yield {
            DealClosed(deal.id)
        } getOrElse {
            DealNotFound(command.dealId)
        }
    }
}