package u04.monads

import Monads.*, Monad.*
import u03.extensionmethods.Streams.*, Stream.*

object States:

  // data structure for state (evolution)
  // a state is/has a function evolving S and producing a result A
  case class State[S, A](run: S => (S, A))

  // minimal set of algorithms
  object State:
    // a facility to run the state on an initial `s`
    extension [S, A](m: State[S, A])
      def apply(s: S): (S, A) = m match
        case State(run) => run(s)
  
  // define a given instance that works on all S, shall use "type lambdas"
  given stateMonad[S]: Monad[[A] =>> State[S, A]] with
    // unit: a state with no evolution, just the result
    def unit[A](a: A): State[S, A] = State(s => (s, a))
    
    // flatMap: runs the state, use result to create a new state
    extension [A](m: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State(s => m.apply(s) match
          case (s2, a) => f(a).apply(s2)
        )
    
// some examples
import States.*
object Counter:
  // a simple state: a counter

  def inc: State[Int, Unit] = State(c => (c + 1, ()))
  def dec: State[Int, Unit] = State(c => (c - 1, ()))
  def get: State[Int, Int] = State(c => (c, c))

@main def testStates(): Unit =
  import Counter.*
  // create a state that increments, then gets the count
  val program: State[Int, Int] = for
    _ <- inc
    _ <- inc
    _ <- dec
    n <- get
  yield n

  // run the program on an initial counter
  val (finalState, result) = program(0)
  println(s"Final state: $finalState, Result: $result")

// some complex state: a simple bank account
object BankAccount:
  case class Account(balance: Double)

  def deposit(amount: Double): State[Account, Unit] = State(a => (Account(a.balance + amount), ()))
  def withdraw(amount: Double): State[Account, Unit] = State(a => (Account(a.balance - amount), ()))
  def getBalance: State[Account, Double] = State(a => (a, a.balance))
  
@main def testBankAccount(): Unit =
  import BankAccount.*
  // create a state that deposits, withdraws, then gets the balance
  val program: State[Account, Double] = for
    _ <- deposit(100)
    _ <- withdraw(30)
    balance <- getBalance
    _ <- withdraw(10)
  yield balance

  // run the program on an initial account
  val (finalAccount, finalBalance) = program(Account(0))
  println(s"Final account: $finalAccount, Final balance: $finalBalance")
