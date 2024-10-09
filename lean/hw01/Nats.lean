--==================================
-- Definições:
--==================================
inductive Nats
| O : Nats
| S : Nats → Nats

def add : Nats → Nats → Nats
| n, Nats.O => n
| n, Nats.S m => Nats.S (add n m)

instance : HAdd Nats Nats Nats where
  hAdd := add
--==================================
-- Axiomas:
--==================================
axiom NA_Ass (a b c: Nats) : (a + b) + c = a + (b + c)
