--==================================
-- Definições:
--==================================
inductive Nats
| O : Nats
| S : Nats → Nats
deriving Repr

open Nats

def add : Nats → Nats → Nats
| n, O => n
| n, S m => S (add n m)

def mul : Nats → Nats → Nats
| n, O => O
| n, S m => add n (mul n m)

def pow : Nats → Nats → Nats
| n, O => S O
| n, S m => mul n (pow n m)

instance : HAdd Nats Nats Nats where
  hAdd := add
instance : HMul Nats Nats Nats where
  hMul := mul
instance : HPow Nats Nats Nats where
  hPow := pow
--==================================
-- Axiomas:
--==================================
axiom NA_Ass (a b c: Nats) : (a + b) + c = a + (b + c)
axiom NA_idR (a : Nats) : a = a + O
--axiom NA_invR (a : Nats) : O = a + (-a)
axiom NA_Com (a b : Nats) : a + b = b + a

axiom NM_Ass (a b c: Nats) : (a * b) * c = a * (b * c)
axiom NM_idR (a : Nats) : a = a * (S O)
axiom NM_Com (a b : Nats) : a * b = b * a

axiom NE_idR (a : Nats) : a = a ^ (S O)


--==================================
-- Lemmas da Esquerda:
--==================================
theorem NM_idL: ∀ a : Nats, a = (S O) * a := by
  intro a
  have h: a = a * (S O) := NM_idR a
  rw [NM_Com a (S O)] at h
  exact h
