unit Calculate;

// Author: Kees Hemerik
// Last modified: 2005 03 18
//
// This unit contains some simple routines for computing the roots of a
// quadratic equation.
// Note: no provisions taken for arithmetic underflow/overflow

interface

function F(A, B, C, X: Real): Real;
{pre: true}
{ret: A*X^2+B*X+C}

function Discr(A, B, C: Real): Real;
// Discriminator
{pre: true}
{ret: B*B - 4*A*C}

procedure VKV(A, B, C: Real; var W1, W2: Real);
// Roots of A*X^2+B*X+C = 0
{pre : A <> 0, 0 <= Discr(A, B, C)}
{post: (forall X: A*X^2+B*X+C = A*(X-W1)*(X-W2) )}

implementation //===============================================================

function F(A, B, C, X: Real): Real;
begin
  {true}
   F := A*X*X + B*X +C;
  {F = A*X^2+B*X+C}
end;

function Discr(A, B, C: Real): Real;
// Discriminator
begin
  {true}
  Discr := B*B - 4*A*C
  {Discr = B*B - 4*a*c}
end;


procedure VKV(A, B, C: Real; var W1, W2: Real);
// Roots of A*X^2+B*X+C = 0
var
  H: Real;
begin
  {Check preconditions}
  Assert(A <> 0, 'VKV.pre not satisfied: A = 0');
  Assert(0 <= Discr(A, B, C), 'VKV.pre not satisfied: Discr(A,B,C) < 0');
  {A <> 0, 0 <= Discr(A, B, C)}
  H := Sqrt(Discr(A, B, C));
  W1 := (- B - H)/(2 * A);
  W2 := (- B + H)/(2 * A);
  {(forall X: A*X^2+B*X+C = A*(X-W1)*(X-W2) )}
end;



end.
 