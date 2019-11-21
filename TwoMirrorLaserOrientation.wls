#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

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

(* First mirror is at a position (x0,y0,z0), which are all fixed*)

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

"\[Gamma]1 and \[Beta]1 are confined by the fact that by definition, the first mirror must send the laser
out to the center of the second mirror. "


"The total rotation of mirror 1 is their product, Rz1.Ry1. The order matters b/c the y-rotation 
must happen first."

rTot1[\[Gamma]1_,\[Beta]1_]:=rz1[\[Beta]1].ry1[\[Gamma]1]
(*rTot1[\[Gamma]1_,\[Beta]1_]:=ry1[\[Gamma]1].rz1[\[Beta]1]*)

MatrixForm[rTot1[\[Gamma]1,\[Beta]1]]

(*How the position of the laser changes due to both rotations*)

nBoth1[\[Gamma]1_,\[Beta]1_]:=rTot1[\[Gamma]1,\[Beta]1].n1
nBoth1[\[Gamma]1,\[Beta]1];

dotlnBoth1[\[Gamma]1_,\[Beta]1_]:=Transpose[nBoth1[\[Gamma]1,\[Beta]1]].lin1
dotlnBoth1[\[Gamma]1,\[Beta]1];

loutBoth1[\[Gamma]1_,\[Beta]1_]:=lin1-2*(dotlnBoth1[\[Gamma]1,\[Beta]1][[1,1]])*nBoth1[\[Gamma]1,\[Beta]1];
Style[Simplify[loutBoth1[\[Gamma]1,\[Beta]1]],FontColor->Purple]
(*Style[Simplify[loutBoth1[-45\[Degree],0\[Degree]]],FontColor->Purple]*)

(* This is the path that the laser takes once it has left the first mirror; this is what
will enter the second mirror.*)

(* The second mirror is at position (xi,yi,zi), with zi being constant and xi and yi being
variables dependent on gamma1 and beta1*)

lin2[\[Gamma]1_,\[Beta]1_]=loutBoth1[\[Gamma]1,\[Beta]1];

n2={
 {0},
 {1},
 {0}
};

"The second mirror can rotate around the x-axis by angle \[Alpha]2 and around the z-axis by \[Beta]2 "
rx2[\[Alpha]2_]:={
 {1, 0, 0},
 {0, Cos[\[Alpha]2], -Sin[\[Alpha]2]},
 {0, Sin[\[Alpha]2], Cos[\[Alpha]2]}
}

rz2[\[Beta]2_]:={
 {Cos[\[Beta]2], -Sin[\[Beta]2], 0},
 {Sin[\[Beta]2], Cos[\[Beta]2], 0},
 {0, 0, 1}
}

rTot2[\[Alpha]2_,\[Beta]2_]:=rz2[\[Beta]2].rx2[\[Alpha]2];

MatrixForm[rTot2[\[Alpha]2,\[Beta]2]]

nBoth2[\[Alpha]2_,\[Beta]2_]:=rTot2[\[Alpha]2,\[Beta]2].n2
nBoth2[\[Alpha]2,\[Beta]2];


(*dotlnBoth2[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]:=Transpose[nBoth2[\[Alpha]2,\[Beta]2]].loutBoth1[\[Gamma]1,\[Beta]1]*)
dotlnBoth2[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]:=Transpose[nBoth2[\[Alpha]2,\[Beta]2]].lin2[\[Gamma]1,\[Beta]1]
dotlnBoth2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2];

(*loutBoth2[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]:=loutBoth1[\[Gamma]1,\[Beta]1]-2*(dotlnBoth2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2][[1,1]])*nBoth2[\[Alpha]2,\[Beta]2]*)
loutBoth2[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]:=lin2[\[Gamma]1,\[Beta]1]-2*(dotlnBoth2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2][[1,1]])*nBoth2[\[Alpha]2,\[Beta]2]

(*lout=lin-2(n*lin)*lin*)

Style[Simplify[loutBoth2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]],FontColor->Black]

Style[Simplify[loutBoth2[0\[Degree],45\[Degree],-45\[Degree],0\[Degree]]],FontColor->Purple];

"We now have the path of the laser after it has been reflected by both mirrors. Now we will see where
it lands at the height of the beam pipe zp."

z1[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]:=Simplify[loutBoth2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2][[3,1]]]
"z1[\[Alpha],\[Beta]]"
z1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2];
x1[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]:=Simplify[loutBoth2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2][[1,1]]]
"x1[\[Alpha],\[Beta]]"
x1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2];
y1[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]:=Simplify[loutBoth2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2][[2,1]]]
"y1[\[Alpha],\[Beta]]"
y1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2];

zAdj[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_]:=zp/z1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]
"zAdj[\[Alpha],\[Beta],zp]"
zAdj[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp];

loutzp[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_]:=loutBoth2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]*zAdj[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp]
"loutzp"
Style[Simplify[loutzp[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp]],FontColor-> Red];

"How the angles affect the positions as x and y prime"

xPrime[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_]:=Simplify[loutzp[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp][[1,1]]]
"xPrime[\[Alpha],\[Beta],zp]"
xPrime[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp];

yPrime[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_]:=Simplify[loutzp[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp][[2,1]]]
"yPrime[\[Alpha],\[Beta],zp]"
yPrime[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp];

"We now have the x and y offsets due to the rotation of the mirrors. With this, we can get 
the total offset from the position of the beam pipe, [xp,yp,zp]. This is of the form 
deltaX=xi+xPrime-xp
deltaY=yi+yPrime-yp
Where [xi,yi,zi] is the position of the final mirror where the laser hits"

deltaX[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,xi_,xp_]:=xi+xPrime[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp]-xp
"deltaX[\[Alpha],\[Beta],zp,xi,xp]"
deltaX[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp];

deltaY[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,yi_,yp_]:=yi+yPrime[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp]-yp
"deltaY[\[Alpha],\[Beta],zp,yi,yp]"
deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp];

"Partials of deltaX"

ddeltaXxp[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,xi_,xp_]:=D[deltaX[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp],xp]
"ddeltaXxp"
ddeltaXxp[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp];

ddeltaXxi[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,xi_,xp_]:=D[deltaX[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp],xi]
"ddeltaXxi"
ddeltaXxi[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp];

ddeltaXzp[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,xi_,xp_]:=D[deltaX[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp],zp]
"ddeltaXzp"
ddeltaXzp[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp];

ddeltaXgamma1[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,xi_,xp_]:=D[deltaX[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp],\[Gamma]1]
"ddeltaXbeta"
ddeltaXgamma1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp];

ddeltaXbeta1[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,xi_,xp_]:=D[deltaX[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp],\[Beta]1]
"ddeltaXbeta"
ddeltaXbeta1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp];

ddeltaXalpha2[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,xi_,xp_]:=D[deltaX[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp],\[Alpha]2]
"ddeltaXalpha"
ddeltaXalpha2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp];

ddeltaXbeta2[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,xi_,xp_]:=D[deltaX[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp],\[Beta]2]
"ddeltaXbeta"
ddeltaXbeta2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp];


"Partials of delta y"

ddeltaYyp[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,yi_,yp_]:=D[deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp],yp]
"ddeltaYyp"
ddeltaYyp[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp];

ddeltaYyi[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,yi_,yp_]:=D[deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp],yi]
"ddeltaYyi"
ddeltaYyi[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp];

ddeltaYzp[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,yi_,yp_]:=D[deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp],zp]
"ddeltaYzp"
ddeltaYzp[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp];

ddeltaYgamma1[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,yi_,yp_]:=D[deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp],\[Gamma]1]
"ddeltaYbeta"
ddeltaYgamma1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp];

ddeltaYbeta1[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,yi_,yp_]:=D[deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp],\[Beta]1]
"ddeltaYbeta"
ddeltaYbeta1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp];

ddeltaYalpha2[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,yi_,yp_]:=D[deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp],\[Alpha]2]
"ddeltaYalpha"
ddeltaYalpha2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp];

ddeltaYbeta2[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,yi_,yp_]:=D[deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp],\[Beta]2]
"ddeltaYbeta"
ddeltaYbeta2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp];

"We combine these partial derivitives to get the error propagtions for deltaX and deltaY"
"These are set as = not := because constants cannot be differentiated"

(*sigmadeltaX[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,xi_,xp_]=Sqrt[((ddeltaXxp[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp])^2*\[Sigma]xp^2+
		(ddeltaXxi[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp])^2*\[Sigma]xi^2+
		(ddeltaXzp[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp])^2*\[Sigma]zp^2+
		(ddeltaXgamma1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp])^2*\[Sigma]Gamma1^2+
		(ddeltaXbeta1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp])^2*\[Sigma]Beta1^2+
		(ddeltaXalpha2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp])^2*\[Sigma]Alpha2^2+
		(ddeltaXbeta2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp])^2*\[Sigma]Beta2^2)];
		
Simplify[sigmadeltaX[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp]]*)

(*sigmadeltaY[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,zp_,yi_,yp_]=Sqrt[((ddeltaYyp[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp])^2*\[Sigma]yp^2+
		(ddeltaYyi[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp])^2*\[Sigma]yi^2+
		(ddeltaYzp[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp])^2*\[Sigma]zp^2+
		(ddeltaYgamma1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp])^2*\[Sigma]Gamma1^2+
		(ddeltaYbeta1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp])^2*\[Sigma]Beta1^2+
		(ddeltaYalpha2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp])^2*\[Sigma]Alpha2^2+
		(ddeltaYbeta2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp])^2*\[Sigma]Beta2^2)];
		
Simplify[sigmadeltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp]]*)

(* These are crazy long and complicated *)


"Turn equations for delta x/y to xi/yi, to get equations for position in terms of angles."

"Simplify[deltaX[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp]]"
Simplify[deltaX[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp]];
"Simplify[Solve[deltaX[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp]\[Equal]0,xi]]"
"xiFunc"
xiFunc=Simplify[Solve[deltaX[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,xi,xp]==0,xi]];

(*This is taken from output; if previous stuff changes, need to reneter this*)
xiFunc=(zp-2 xp Cos[\[Alpha]2] Sin[\[Alpha]2] Sin[\[Beta]2]-2 zp Cos[\[Alpha]2]^2 Sin[\[Beta]2]^2+
	2 Cos[\[Beta]1]^2 Cos[\[Gamma]1]^2 (xp Sin[2 \[Alpha]2] Sin[\[Beta]2]+zp (-1+2 Cos[\[Alpha]2]^2 Sin[\[Beta]2]^2))-
	Cos[\[Beta]1] (4 xp Cos[\[Alpha]2] Cos[\[Beta]2] Cos[\[Gamma]1]^2 Sin[\[Alpha]2] Sin[\[Beta]1]+
	4 zp Cos[\[Alpha]2]^2 Cos[\[Beta]2] Cos[\[Gamma]1]^2 Sin[\[Beta]1] Sin[\[Beta]2]+(xp Cos[2 \[Alpha]2]-
	zp Sin[2 \[Alpha]2] Sin[\[Beta]2]) Sin[2 \[Gamma]1]))/(2 Cos[\[Alpha]2] Sin[\[Alpha]2] 
	(-2 Cos[\[Beta]1] Cos[\[Beta]2] Cos[\[Gamma]1]^2 Sin[\[Beta]1]-Sin[\[Beta]2]+2 Cos[\[Beta]1]^2 
	Cos[\[Gamma]1]^2 Sin[\[Beta]2])-Cos[2 \[Alpha]2] Cos[\[Beta]1] Sin[2 \[Gamma]1]);

"Simplify[deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp]]"
Simplify[deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp]];
"Simplify[Solve[deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp]==0,yi]]"
"yiFunc"
yiFunc=Simplify[Solve[deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp]==0,yi]];

yiFunc=(Cos[\[Alpha]2] (-2 yp Sin[\[Alpha]2] Sin[\[Beta]2]+zp Cos[\[Alpha]2] Sin[2 \[Beta]2])-2 Cos[\[Alpha]2] 
	Cos[\[Beta]1]^2 Cos[\[Gamma]1]^2 (-2 yp Sin[\[Alpha]2] Sin[\[Beta]2]+zp Cos[\[Alpha]2] Sin[2 \[Beta]2])+
	Cos[\[Beta]1] (2 Cos[\[Gamma]1]^2 (-zp+2 zp Cos[\[Alpha]2]^2 Cos[\[Beta]2]^2-2 yp Cos[\[Alpha]2] Cos[\[Beta]2] 
	Sin[\[Alpha]2]) Sin[\[Beta]1]-4 zp Cos[\[Alpha]2] Cos[\[Beta]2] Cos[\[Gamma]1] Sin[\[Alpha]2] Sin[\[Gamma]1]-yp Cos[2 \[Alpha]2] 
	Sin[2 \[Gamma]1]))/(2 Cos[\[Alpha]2] Sin[\[Alpha]2] (-2 Cos[\[Beta]1] Cos[\[Beta]2] Cos[\[Gamma]1]^2 Sin[\[Beta]1]-Sin[\[Beta]2]+
	2 Cos[\[Beta]1]^2 Cos[\[Gamma]1]^2 Sin[\[Beta]2])-Cos[2 \[Alpha]2] Cos[\[Beta]1] Sin[2 \[Gamma]1]);

(*\[Beta]1=45\[Degree]
\[Gamma]1=0\[Degree]
Simplify[xiFunc]
Simplify[yiFunc]*)

(* These seem to match the results for the single mirror*)


"Get partials of xi/yi to get error propagation of mirror position with respect to angles"

"dxis"
"xp"
dxixp=D[xiFunc,xp];
"zp"
dxizp=D[xiFunc,zp];
"gamma1"
dxiGamma1=D[xiFunc,\[Gamma]1];
"beta1"
dxiBeta1=D[xiFunc,\[Beta]1];
"alpha2"
dxiAlpha2=D[xiFunc,\[Alpha]2];
"beta2"
dxiBeta2=D[xiFunc,\[Beta]2];

(*"Sigma xi"
\[Sigma]xi=Simplify[Sqrt[dxixp^2*\[Sigma]xi^2+dxizp^2*\[Sigma]zp^2+dxiGamma1^2*\[Sigma]Gamma1^2+dxiBeta1^2*\[Sigma]Beta1^2+
	dxiAlpha2^2*\[Sigma]Alpha2^2+dxiBeta2^2*\[Sigma]Beta2^2]]*)
	
(* This result is crazy long, the result is saved in a text file *)

"dyis"
"yp"
dyiyp=D[yiFunc,yp];
"zp"
dyizp=D[yiFunc,zp];
"gamma1"
dyiGamma1=D[yiFunc,\[Gamma]1];
"beta1"
dyiBeta1=D[yiFunc,\[Beta]1];
"alpha2"
dyiAlpha2=D[yiFunc,\[Alpha]2];
"beta2"
dyiBeta2=D[yiFunc,\[Beta]2];

(*"Sigma yi"
\[Sigma]yi=Simplify[Sqrt[dyiyp^2*\[Sigma]yi^2+dyizp^2*\[Sigma]zp^2+dyiGamma1^2*\[Sigma]Gamma1^2+dyiBeta1^2*\[Sigma]Beta1^2+
	dyiAlpha2^2*\[Sigma]Alpha2^2+dyiBeta2^2*\[Sigma]Beta2^2]]*)
	
	
"TPC Coordinates; this assumes the beam enters directly into the base of the TPC. To account for the beam pipe, there will need to be 
a term to determine how many times it reflects in the beam pipe to see where it actually points when it leaves the pipe. I think this should just affect the
quadrant/signs of the angles"

Remove[\[Theta]]
Remove[\[Phi]]

\[Theta][\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=ArcCos[z1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]/Sqrt[x1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]^2+y1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]^2+
							z1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]^2]];
\[Theta][\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=Simplify[\[Theta][\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]];
\[Phi][\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=ArcTan[x1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2],y1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]];
\[Phi][\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=Simplify[\[Phi][\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]];

(*

"Error Propagation for theta(gamma1,beta1,alpha2,beta2)"
\[Sigma]ThetaGamma1[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=D[\[Theta][\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2],\[Gamma]1];
Simplify[\[Sigma]ThetaGamma1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]];
\[Sigma]ThetaBeta1[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=D[\[Theta][\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2],\[Beta]1];
Simplify[\[Sigma]ThetaBeta1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]];
\[Sigma]ThetaAlpha2[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=D[\[Theta][\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2],\[Alpha]2];
Simplify[\[Sigma]ThetaAlpha2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]];
\[Sigma]ThetaBeta2[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=D[\[Theta][\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2],\[Beta]2];
Simplify[\[Sigma]ThetaBeta2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]];

\[Sigma]Theta[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=Sqrt[\[Sigma]Gamma1^2\[Sigma]ThetaGamma1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]^2+
	\[Sigma]Beta1^2\[Sigma]ThetaBeta1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]^2+
	\[Sigma]Alpha2^2\[Sigma]ThetaAlpha2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]^2+
	\[Sigma]Beta2^2\[Sigma]ThetaBeta2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]^2];
"Simplify[\[Sigma]Theta[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]]"
Simplify[\[Sigma]Theta[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]];


"Error Propagation for phi(alpha,beta)"
\[Sigma]PhiGamma1[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=D[\[Phi][\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2],\[Gamma]1];
Simplify[\[Sigma]PhiGamma1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]];
\[Sigma]PhiBeta1[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=D[\[Phi][\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2],\[Beta]1];
Simplify[\[Sigma]PhiBeta1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]];
\[Sigma]PhiAlpha2[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=D[\[Phi][\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2],\[Alpha]2];
Simplify[\[Sigma]PhiAlpha2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]];
\[Sigma]PhiBeta2[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=D[\[Phi][\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2],\[Beta]2];
Simplify[\[Sigma]PhiBeta2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]];

\[Sigma]Phi[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]=Sqrt[\[Sigma]Gamma1^2\[Sigma]PhiGamma1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]^2+
	\[Sigma]Beta1^2\[Sigma]PhiBeta1[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]^2+
	\[Sigma]Alpha2^2\[Sigma]PhiAlpha2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]^2+
	\[Sigma]Beta2^2\[Sigma]PhiBeta2[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]^2];
"Simplify[\[Sigma]Theta[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]]"
Simplify[\[Sigma]Phi[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]];

(* The xi and yi positions of the second mirror are contained in the \[Alpha]2 and \[Beta]2 terms. *)
*)

"To get the error coefficients in terms of theta and phi"
"The first mirror rotates in two directions; it rotates around the y-axis by angle \[Gamma]1 and it 
rotates around the z-axis by angle \[Beta]1. To reflect this, we rotate the normal vector of the mirror
with the rotation matrices"


(*xi=x0-(y0-yi)Tan[90\[Degree]-2\[Beta]1]
yi=y0-Sqrt[(z0-zi)^2(Tan[90\[Degree]-2\[Gamma]1])^2-(x0-xi)^2]*)

x0=-2.5;
y0=-6.936;
z0=0.2165;

zi=0;

\[Beta]1=(90\[Degree]-ArcTan[xi-x0/yi-y0])/2;
\[Gamma]1=(90\[Degree]-ArcTan[Sqrt[(x0-xi)^2+(y0-yi)^2]/z0-zi])/2;



Simplify[xiFunc]
Simplify[yiFunc]

xiFunc0=xiFunc-xi
yiFunc0=yiFunc-yi

Solve[xiFunc0==0,xi]
Solve[yiFunc0==0,yi]



(*
(* Can get xPrime and yPrime in terms of xi,yi,alpha2,beta2 *)

xPrimeC[xi_,yi_,\[Alpha]2_,\[Beta]2_]:=Simplify[xPrime[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp]]
xPrimeC[xi,yi,\[Alpha]2,\[Beta]2]
yPrimeC[xi_,yi_,\[Alpha]2_,\[Beta]2_]:=Simplify[yPrime[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp]]
yPrimeC[xi,yi,\[Alpha]2,\[Beta]2]

(* Can get deltaX and Y in terms of xi, xPrime, which is in terms of xi, *)


deltaXC[xi_,yi_,\[Alpha]2_,\[Beta]2_]:=xi+xPrimeC[xi,yi,\[Alpha]2,\[Beta]2]-xp
"deltaX[\[Alpha],\[Beta],zp,xi,xp]"
deltaXC[xi,yi,\[Alpha]2,\[Beta]2]

deltaYC[xi_,yi_,\[Alpha]2_,\[Beta]2_]:=yi+yPrimeC[xi,yi,\[Alpha]2,\[Beta]2]-yp
"deltaY[\[Alpha],\[Beta],zp,yi,yp]"
deltaYC[xi,yi,\[Alpha]2,\[Beta]2]


(*"xiFunc"
Simplify[xiFunc]*)

"xiFunc"
xiFuncC=FullSimplify[Solve[deltaXC[xi,yi,\[Alpha]2,\[Beta]2]==0,xi]]

(*
"Simplify[deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp]]"
Simplify[deltaYC[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp]];
"Simplify[Solve[deltaY[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,zp,yi,yp]==0,yi]]"
"yiFunc"
yiFuncC=Simplify[Solve[deltaYC[xi,yi,\[Alpha]2,\[Beta]2]==0,yi]];*)*)




