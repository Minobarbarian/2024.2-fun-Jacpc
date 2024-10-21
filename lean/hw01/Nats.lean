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

  --Identidade Direita da Soma
  theorem NA_idR: ∀ n : Nats, n + O = n := by
    intro n
    rw[plus]

  --Sucessor Direito da Soma
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

  --Identidade Esquerda da Soma
  theorem NA_idL: ∀ n : Nats, O + n = n := by
    intro n
    induction n with
    |O => rfl
    |S n ih =>
      rw [NA_SR]
      rw [ih]

  --Sucessor Esquerdo da Soma
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

--Multiplicar por Zero pela Direita
theorem NM_OR: ∀ n : Nats, n * O = O := by
  intro n
  rw[times]

--Sucessor Direito da Multiplicação
theorem NM_SR: ∀ (n m : Nats), n * S m = n + (n * m) := by
  intro n m
  rw[times]

--Identidade Direita da Multiplicação
theorem NM_idR: ∀ n : Nats, n * S O = n := by
  intro n
  rw [times]
  rw [NM_OR]
  rw [NA_idR]

theorem NM_Distr: ∀ (a b c: Nats), a * (b + c) = a * b + a * c := by
  intro a b c
  induction c with
  |O =>
    rw[NA_idR]
    rw[NM_OR]
    rw[NA_idR]
  |S b ih =>
    rw[NA_SR]
    rw[NM_SR]
    rw[ih]
    rw[NM_SR]
    rw[NA_Com]
    rw[NA_Ass]
    rw[NA_Com]

--Associatividade da Multiplicação
theorem NM_Ass: ∀ (a b c: Nats), (a * b) * c = a * (b * c) := by
  intro a b c
  induction c with
  |O =>
    rw[NM_OR]
    rw[NM_OR]
    rw[NM_OR]
  |S c ih =>
    rw[NM_SR]
    rw[NM_SR]
    rw[ih]

--Comutatividade da Multiplicação
theorem NM_Com: ∀ (n m : Nats), m * n = n * m
