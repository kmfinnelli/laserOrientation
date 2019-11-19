#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global"]

(*
This is the code to determine how the final position of a laser is altered depending on two 
mirrors. The laser hits the first mirror, which is stationary but can rotate with angles \[Gamma]1 and 
\[Beta]1. This sends the laser to the second mirror which can move in x and y and rotate with \[Alpha]2 and 
\[Beta]2. This mirror sends the laser into the beam pipe, which is fixed. The second mirror cna be
moved in order to change the angle of the light going into the beam pipe. The mirror can move at 
most 2.5 in the both the x and y dimensions. From this we can get the angles \[Theta] and \[Phi] that the
laser travels inside the TPC as functions of the mirror angles. We can also do the error 
propagation for this in order to see how precise they must be to get certain angles.
*)

"First Mirror"

"lin1: the incoming path of the laser [x,y,z] going in to mirror 1. Only in the x-direction";
lin1={
 {-1},
 {0},
 {0}
};
"n1:the normal vector of mirror 1 at the starting position, which is completely vertical facing
the x-direction, the incoming path of the laser. ";
n1={
 {1},
 {0},
 {0}
};

"The first mirror rotates in two directions; it rotates around the y-axis by angle \[Gamma]1 and it 
rotates around the z-axis by angle \[Beta]1. To reflect this, we rotate the normal vector of the mirror
with the rotation matrices";

rz1[\[Beta]1_]:={
 {Cos[\[Beta]1], -Sin[\[Beta]1], 0},
 {Sin[\[Beta]1], Cos[\[Beta]1], 0},
 {0, 0, 1}
}

ry1[\[Gamma]1_]:={
 {Cos[\[Gamma]1], 0, Sin[\[Gamma]1]},
 {0, 1, 0},
 {-Sin[\[Gamma]1], 0, Cos[\[Gamma]1]}
}

"The total rotation of mirror 1 is their product, Rz1.Ry1. The order matters b/c the y-rotation 
must happen first."

rTot1[\[Gamma]1_,\[Beta]1_]:=rz1[\[Beta]1].ry1[\[Gamma]1]
(*rTot1[\[Gamma]1_,\[Beta]1_]:=ry1[\[Gamma]1].rz1[\[Beta]1]*)

MatrixForm[rTot1[\[Gamma]1,\[Beta]1]]

(*How the position of the laser changes due to both rotations*)

nBoth1[\[Gamma]1_,\[Beta]1_]:=rTot1[\[Gamma]1,\[Beta]1].n1
nBoth1[\[Gamma]1,\[Beta]1]

dotlnBoth1[\[Gamma]1_,\[Beta]1_]:=Transpose[nBoth1[\[Gamma]1,\[Beta]1]].lin1
dotlnBoth1[\[Gamma]1,\[Beta]1]

loutBoth1[\[Gamma]1_,\[Beta]1_]:=lin1-2*(dotlnBoth1[\[Gamma]1,\[Beta]1][[1,1]])*nBoth1[\[Gamma]1,\[Beta]1];
Style[Simplify[loutBoth1[\[Gamma],\[Beta]]],FontColor->Purple]
(*Style[Simplify[loutBoth1[-45\[Degree],0\[Degree]]],FontColor->Purple]*)
\!\(\*
StyleBox["\n",
StripOnInput->False,
FontColor->RGBColor[0.5, 0, 0.5]]\)(* This is the path that the laser takes once it has left the first mirror; this is what
will enter the second mirror.*)\!\(\*
StyleBox["\n",
StripOnInput->False,
FontColor->RGBColor[0.5, 0, 0.5]]\)
lin2[\[Gamma]1_\[Beta]1 _]:=loutBoth1[\[Gamma]1,\[Beta]1]

n2={
 {0},
 {-1},
 {0}
}





