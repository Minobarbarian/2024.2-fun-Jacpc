--==================================
-- Definições:
--==================================
inductive Nats
| O : Nats
| S : Nats → Nats
deriving BEq, DecidableEq, Inhabited

open Nats

def plus : Nats → Nats → Nats
  | n, O => n
  | n, S m => S (plus n m)
infixl: 70 "+" => plus

def times : Nats → Nats → Nats
| n, O => O
| n, S m => n + (times n m)
infixl: 80 "*" => times

def pow : Nats → Nats → Nats
| n, O => S O
| n, S m => n * (pow n m)
infixl: 90 "^" => pow

--==================================
-- Teoremas de plus:
--==================================

  --Identidade Direita
  theorem NA_idR: ∀ n : Nats, n + O = n := by
    intro n
    rw[plus]

  --Sucessor Direito
  theorem NA_SR: ∀ (n m : Nats), n + S m = S (n + m) := by
    intro n m
    rw[plus]

  --Associatividade da Soma
  theorem NA_Ass: ∀ (a b c: Nats), (a + b) + c = a + (b + c) := by
    intro a b c
    induction c with
    |O =>
      rw[NA_idR]
      rw[NA_idR]
    |S c ih =>
      rw[NA_SR]
      rw[NA_SR]
      rw[NA_SR]
      rw[ih]

  --Identidade Esquerda
  theorem NA_idL: ∀ n : Nats, O + n = n := by
    intro n
    induction n with
    |O => rfl
    |S n ih =>
      rw [NA_SR]
      rw [ih]

  --Sucessor Esquerdo
  theorem NA_SL: ∀ (n m : Nats), S n + m = S (n + m) := by
    intro n m
    induction m with
    |O =>
      rw[NA_idR]
      rw[NA_idR]
    |S n ih =>
      rw[NA_SR]
      rw[ih]
      rw[NA_SR]

  --Comutatividade da Soma
  theorem NA_Com: ∀ (n m : Nats), m + n = n + m := by
    intro n m
    induction n with
    |O =>
      rw[NA_idL]
      rw[NA_idR]
    |S n ih =>
      rw[NA_SR]
      rw[NA_SL]
      rw[ih]

--==================================
-- Teoremas de times:
--==================================

--Identidade Direita
theorem NM_idR: ∀ n : Nats, n * S O = n := by
  intro n
  rw[]
