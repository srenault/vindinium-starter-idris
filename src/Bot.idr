module Bot

import Effect.Random
import Model

Bot : Type
Bot = Input -> Direction

--defaultBot : Input -> Eff (Maybe Direction)
--defaultBot _ = rndSelect [Stay, North, South, East, West]
