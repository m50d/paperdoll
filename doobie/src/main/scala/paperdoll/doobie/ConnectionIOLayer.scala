package paperdoll.doobie

import paperdoll.core.effect.Effects.sendU
import paperdoll.core.effect.Effects
import doobie.imports.ConnectionIO

object ConnectionIOLayer {
  def sendConnectionIO[A](connectionIO: ConnectionIO[A]): Effects.One[ConnectionIO_, A] =
    sendU(connectionIO)
}