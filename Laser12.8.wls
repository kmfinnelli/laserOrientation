#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

(*Positions*)

(* Position of first stationary mirror*)
x0=2.53 ;
y0=1.03 ;
z0=4.19 ;


(*Positions of second moving mirror; z1 is constant, x and y can move between their home
and max positions *)
z1=3.9721 ;

x1Home=5.03 ;
x1Max=2.53 ;

y1Home=7.97 ;
y1Max=10.47 ;

Mhome={x1Home,y1Home,z1};
MxMax={x1Max,y1Home,z1};
MyMax={x1Home,y1Max,z1};
MxyMax={x1Max,y1Max,z1};

(*Position of beam pipe*)
xp=4.87 ;
yp=8.13 ;
zp=4.19 ;

Mp={xp,yp,zp};


(*Going backward*)

(* Position of mirror 2 in terms of theta and phi in the TPC*)

(*"x1=" 
x1[\[Theta]_,\[Phi]_]:=xp-((zp-z1)/Cos[\[Theta]])*Sin[\[Theta]]Cos[\[Phi]];
"y1=" 
y1[\[Theta]_,\[Phi]_]:=yp-((zp-z1)/Cos[\[Theta]])*Sin[\[Theta]]Sin[\[Phi]];*)

"x1="
x1[\[Theta]_,\[Phi]_]:=xp-(zp-z1)*Tan[\[Theta]]Cos[\[Phi]];
"y1=" 
y1[\[Theta]_,\[Phi]_]:=yp-(zp-z1)*Tan[\[Theta]]Sin[\[Phi]];


(*Path of the laser that goes from the center of mirror 2 to the position of the beam pipe. It
is described in terms of theta and phi, because those determine the path of the beam going 
into the pipe, which must be what comes out of mirror 2*)
"lout2pb=" 
lout2pb={Sin[\[Theta]b]Cos[\[Phi]b],Sin[\[Theta]b]Sin[\[Phi]b],Cos[\[Theta]b]}; (*backwardL2*)

(*Vector Coordinates of mirror 2 in terms of theta and phi*)

(*(*MINE*)
"M2b=" 
M2b={x1[\[Theta]b,\[Phi]b],y1[\[Theta]b,\[Phi]b],z1};*)

(*Ross  . makes #'s worse*)
"M2b="
M2b=Mp+(Mhome[[3]]-Mp[[3]])/lout2pb[[3]]*lout2pb (*backwardM2c*)

(*Vector Coordinates of mirror 1; constant*)
"M1=" 
M1={x0,y0,z0};


(*Path of the laser that goes from the center of mirror 1 and center of mirror 2, 
determined by geometry*)
"lout12b=" 
lout12b=Normalize[M2b-M1]; (*backwardL1c*)

(*The normal vector of mirror 2 can be determined by using these two input and output 
vectors *)
(*"
MINE
n2=" 
n2=(-lout12b+lout2pb)/Sqrt[2-2lout12b.lout2pb];*)

(*Ross*)
"n2="
n2=Normalize[-lout12b+lout2pb]; (*backwardN2c*)

(*The normal vector can also be found in terms of the angles that the mirror can rotate. 
Mirror 2 can rotate along 2 axis, along the x-axis by angle \[Alpha]2 and around the z-axis by
\[Beta]2. By applying the rotation matrices of both these transformations to the unrotated 
normal vector of the mirror, we can find the mirror when it has been rotated in terms of 
the two angles. The original normal vector of mirror 2 is (0,-1,0), because it points in
the negative y-axis*)



(*These can define the M2 angles \[Alpha]2 and \[Beta]2 in terms of \[Theta] and \[Phi]. These depend only on \[Theta] and 
\[Phi], not on x1 or y1*)

(*MINE*)
"\[Alpha]2b="
\[Alpha]2bt=ArcSin[-n2[[3]]];
(*
"\[Beta]2b="
\[Beta]2b=ArcTan[-n2[[2]],n2[[1]]];*)

(*Ross*)

"\[Beta]2b="
\[Beta]2b=ArcCos[Normalize[n2-{0,0,n2[[3]]}].{0,-1,0}]; (*backward\[Beta]2*)
"\[Alpha]2b="
\[Alpha]2b=ArcTan[Sqrt[n2[[1]]^2+n2[[2]]^2],n2[[3]]]; (*backward\[Alpha]2*)

(*The normal vector of M1 can be found looking at the vectors going in and out of it, 
getting the normal vector in terms of \[Theta] and \[Phi]*)

lout01={-1,0,0};

(*MINE
"n1="
n1=(-lout01+lout12b)/Sqrt[2-2lout01.lout12b];*)

(*Ross*)
"n1="
n1=Normalize[-lout01+lout12b]; (*backwardN1c*)




(*MINE
"\[Gamma]1b="
\[Gamma]1b=ArcSin[-n1[[3]]];
"\[Beta]1b="
\[Beta]1b=ArcTan[-n1[[1]],n1[[2]]];*)

(*Ross*)

"\[Beta]1b="
\[Beta]1b=ArcCos[Normalize[n1-{0,0,n1[[3]]}].{1,0,0}]; (*backward\[Beta]1*)
"\[Gamma]1b="
\[Gamma]1b=ArcTan[Sqrt[n1[[1]]^2+n1[[2]]^2],n1[[3]]]; (*backward\[Gamma]1*)


(* TEST VALUES *)

"HOME"

\[Theta]b=46.08\[Degree]
\[Phi]b=135\[Degree]
"\[Theta]b="
\[Theta]b*180/\[Pi]
"\[Phi]b="
\[Phi]b*180/\[Pi]


"M2-homeM2"
M2b-Mhome

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2bt"," "\[Alpha]2bt *180/\[Pi]}]]



"YMAX"

\[Theta]b=84.69230033409917\[Degree];
\[Phi]b=-93.91157181252866\[Degree];
"\[Theta]b="
\[Theta]b*180/\[Pi]
"\[Phi]b="
\[Phi]b*180/\[Pi]

"M2-MyMAx"
M2b-MyMax

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2bt"," "\[Alpha]2bt *180/\[Pi]}]]



"XMAX"

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}]]

(*\[Theta]b=84.69230033409917\[Degree]*)
\[Theta]b=84.6414\[Degree]
\[Phi]b=3.91157\[Degree]

"\[Theta]b="
\[Theta]b*180/\[Pi]
"\[Phi]b="
\[Phi]b*180/\[Pi]

"M2-MxMax"
M2b-MxMax
M2b-Mhome

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2bt"," "\[Alpha]2bt *180/\[Pi]}]]





"XYMAX"

\[Theta]b=86.23276652956393\[Degree]
\[Phi]b=45.0\[Degree]
"\[Theta]b="
\[Theta]b*180/\[Pi]
"\[Phi]b="
\[Phi]b*180/\[Pi]

"M2-MxyMax"
M2b-MxyMax

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}]]



(*Going Forward*)

(*The laser going into M1*)
lout01={-1,0,0};
(*The original normal vector of M1*)
n1Orig={1,0,0};

(* Again, we can find this normal vector in therms of the mirror 1 angles. It rotates around
the y-axis by angle \[Gamma]1 and around the z-axis by angle \[Beta]1*)
ry1=RotationTransform[\[Gamma]1b,{0,1,0}];
rz1=RotationTransform[\[Beta]1b,{0,0,1}];

"nAngle1="
nAngle1=rz1[ry1[{1,0,0}]]; (*forwardN1*)

"lout12f="
lout12f=lout01-2(nAngle1.lout01)*nAngle1; (*forwardL1c*)

(*Now send this laser to hit M2*)
(*The original normal vector of M2*)
n2Orign={0,-1,0}

rx2=RotationTransform[\[Alpha]2b,{1,0,0}];
rz2=RotationTransform[\[Beta]2b,{0,0,1}];

"nAngle2="
nAngle2=rz2[rx2[{0,-1,0}]] (*forwardN2*)

"lout2pf="
lout2pf=lout12f-2(nAngle2.lout12f)*nAngle2; (*forwardL2c*)

(*TPc Coordinates in terms of mirror angles*)

(*MINE
"\[Theta]f="
\[Theta]f=ArcCos[lout2pf[[3]]];
"\[Phi]f="
\[Phi]f=ArcTan[lout2pf[[1]],lout2pf[[2]]];*)

(*Ross*)
"\[Theta]f="
\[Theta]f=ArcTan[lout2pf[[3]],Sqrt[lout2pf[[1]]^2+lout2pf[[2]]^2]] (*forward\[Theta]*)
"\[Phi]f="
\[Phi]f=ArcTan[lout2pf[[1]],lout2pf[[2]]] (*forward\[Phi]*)




(*Test Values*)

SequenceForm[Column[{"\[Degree]=\[Beta]1b", 44.98588619333923 " "}], Column[{"\[Degree]=\[Gamma]1b", (-1.2715313780860467`) " "}], Column[{"\[Degree]=\[Beta]2b", 46.83909527306649 " "}], Column[{"\[Degree]=\[Alpha]2b", 5.197796762909511 " "}], Column[{"\[Degree]=\[Alpha]2bt", (-5.197796762909511) " "}]]

"Xmax"
(*my mirror angles*)
\[Beta]1f=44.98588619333923\[Degree]
\[Gamma]1f=-1.2715313780860467\[Degree]
\[Beta]2f=46.83909527306649\[Degree]
\[Alpha]2f=-5.197796762909511\[Degree]

Print[Column[{"\[Degree]=\[Theta]f"," "\[Theta]f*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]f"," "\[Phi]f*180/\[Pi]}]]
(*
(*Ross's mirror angles*)
\[Beta]1f=37.577829859724346\[Degree]
\[Gamma]1f=-0.8142522235138099\[Degree]
\[Beta]2f=9.383662383506968\[Degree]
\[Alpha]2f=-3.3396843014085085\[Degree]

Print[Column[{"\[Degree]=\[Theta]f"," "\[Theta]f*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]f"," "\[Phi]f*180/\[Pi]}]]*)


(*Putting things together*)

(*Get theta and phi in terms of x1s, and y1s (used to plug in for the specific positions)*)
"\[Theta]s="
\[Theta]s=ArcCos[(zp-z1)/Sqrt[(xp-x1s)^2+(yp-y1s)^2+(zp-z1)^2]];
"\[Phi]s="
\[Phi]s=ArcTan[xp-x1s,yp-y1s];

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2bt"," "\[Alpha]2bt *180/\[Pi]}]]

y1s=y1Home
\[Alpha]2b
\[Theta]b=\[Theta]s
\[Phi]b=\[Phi]s

Plot[\[Alpha]2b*180/\[Pi],{x1s,x1Home,x1Max}]


(*Test Values*)

"Home"
x1s=x1Home
y1s=y1Home

Print[Column[{"\[Degree]=\[Theta]"," "\[Theta]s*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]"," "\[Phi]s*180/\[Pi]}]]

\[Theta]b=\[Theta]s
\[Phi]b=\[Phi]s

"M2-homeM2"
M2b-Mhome

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2bt"," "\[Alpha]2bt *180/\[Pi]}]]

"YMax"
x1s=x1Home
y1s=y1Max

Print[Column[{"\[Degree]=\[Theta]"," "\[Theta]s*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]"," "\[Phi]s*180/\[Pi]}]]

\[Theta]b=\[Theta]s
\[Phi]b=\[Phi]s

"M2-homeM2"
M2b-Mhome

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2bt"," "\[Alpha]2bt *180/\[Pi]}]]

"XMax"
x1s=x1Max
y1s=y1Home

Print[Column[{"\[Degree]=\[Theta]"," "\[Theta]s*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]"," "\[Phi]s*180/\[Pi]}]]

\[Theta]b=\[Theta]s
\[Phi]b=\[Phi]s

"M2-homeM2"
M2b-Mhome

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2bt"," "\[Alpha]2bt *180/\[Pi]}]]

"YXMax"
x1s=x1Max
y1s=y1Max

Print[Column[{"\[Degree]=\[Theta]"," "\[Theta]s*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]"," "\[Phi]s*180/\[Pi]}]]

\[Theta]b=\[Theta]s
\[Phi]b=\[Phi]s

"M2-homeM2"
M2b-Mhome

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2bt"," "\[Alpha]2bt *180/\[Pi]}]]




