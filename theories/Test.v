Require Import Lists.List.
From EasyBakeRust Require Import EasyBakeRust.

Definition x := 2.
CakeML_Extraction x.
Recursive CakeML_Extraction x.

Inductive contains : Type :=
| Cone : nat -> contains
| Ctwo : list nat -> contains.

CakeML_Extraction "test.rs" plus list fst hd map contains.
