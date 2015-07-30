package com.geteit

import scala.concurrent.ExecutionContext

package object events {

  type Publisher[E] = SourceStream[E]

  object Publisher {
    def apply[E](ec: Option[ExecutionContext]) = new SourceStream[E] {
      override val executionContext = ec
    }
  }
}
