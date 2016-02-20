package com.github.m50d.paperdoll

package object reader {
  type Reader_[I] = Layer {
    type F[X] = Reader[I, X]
  }
}