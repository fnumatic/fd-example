package taxcalc

import fnumatic.ebc.{AppFlow}

object Main extends App with AppFlow[Unit,Unit] with RootFlow{
  start()
}



