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
\[Alpha]2b=ArcSin[-n2[[3]]];
"\[Beta]2b="
\[Beta]2b=ArcTan[-n2[[2]],n2[[1]]];

(*(*Ross*)

"\[Beta]2b="
\[Beta]2b=ArcCos[Normalize[n2-{0,0,n2[[3]]}].{0,-1,0}]; (*backward\[Beta]2*)
"\[Alpha]2b="
\[Alpha]2b=ArcTan[Sqrt[n2[[1]]^2+n2[[2]]^2],n2[[3]]]; (*backward\[Alpha]2*)*)

(*The normal vector of M1 can be found looking at the vectors going in and out of it, 
getting the normal vector in terms of \[Theta] and \[Phi]*)

lout01={-1,0,0};

(*MINE
"n1="
n1=(-lout01+lout12b)/Sqrt[2-2lout01.lout12b];*)

(*Ross*)
"n1="
n1=Normalize[-lout01+lout12b]; (*backwardN1c*)

MINE
"\[Gamma]1b="
\[Gamma]1b=ArcSin[-n1[[3]]];
"\[Beta]1b="
\[Beta]1b=ArcTan[n1[[1]],n1[[2]]];


(*(*Ross*)

"\[Beta]1b="
\[Beta]1b=ArcCos[Normalize[n1-{0,0,n1[[3]]}].{1,0,0}]; (*backward\[Beta]1*)
"\[Gamma]1b="
\[Gamma]1b=ArcTan[Sqrt[n1[[1]]^2+n1[[2]]^2],n1[[3]]]; (*backward\[Gamma]1*)*)























(*Going Forward*)

(*The laser going into M1*)
lout01={-1,0,0};
(*The original normal vector of M1*)
n1Orig={1,0,0};

(* Again, we can find this normal vector in therms of the mirror 1 angles. It rotates around
the y-axis by angle \[Gamma]1 and around the z-axis by angle \[Beta]1*)
ry1=RotationTransform[\[Gamma]1f,{0,1,0}];
rz1=RotationTransform[\[Beta]1f,{0,0,1}];

"nAngle1="
nAngle1=rz1[ry1[{1,0,0}]] (*forwardN1*)

(*"nAngle1="
nAngle1=ry1[rz1[{1,0,0}]]; (*forwardN1*)*)


"lout12f="
lout12f=lout01-2(lout01.nAngle1)*nAngle1; (*forwardL1c*)

(*Now send this laser to hit M2*)
(*The original normal vector of M2*)
n2Orign={0,-1,0}

rx2=RotationTransform[\[Alpha]2f,{1,0,0}];
rz2=RotationTransform[\[Beta]2f,{0,0,1}];

"nAngle2="
nAngle2=rz2[rx2[{0,-1,0}]] (*forwardN2*)

(*"nAngle2="
nAngle2=rx2[rz2[{0,-1,0}]] (*forwardN2*)*)

"lout2pf="
lout2pf=lout12f-2(lout12f.nAngle2)*nAngle2; (*forwardL2c*)

M2f=Mp+(Mhome[[3]]-Mp[[3]])/lout2pf[[3]]*lout2pf;

(*R2=IdentityMatrix[1]-2(nAngle2).(Transpose[nAngle2])

lout2pf=R2.lout12f*)

(*TPc Coordinates in terms of mirror angles*)

(*MINE*)
"\[Theta]f="
\[Theta]f=ArcCos[lout2pf[[3]]];
"\[Phi]f="
\[Phi]f=ArcTan[lout2pf[[1]],lout2pf[[2]]];

(*(*Ross*)
"\[Theta]f="
\[Theta]f=ArcTan[lout2pf[[3]],Sqrt[lout2pf[[1]]^2+lout2pf[[2]]^2]]; (*forward\[Theta]*)
"\[Phi]f="
\[Phi]f=ArcTan[lout2pf[[1]],lout2pf[[2]]];(*forward\[Phi]*)*)




(*(*Test Values*)


"Xmax"
(*my mirror angles*)
\[Beta]1f=44.98588619333923\[Degree]
\[Gamma]1f=-1.2715313780860467\[Degree]
\[Beta]2f=46.83909527306649\[Degree]
\[Alpha]2f=-5.197796762909511\[Degree]

Print[Column[{"\[Degree]=\[Theta]f"," "\[Theta]f*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]f"," "\[Phi]f*180/\[Pi]}]]*)



(*Putting things together*)

(*Get theta and phi in terms of x1s, and y1s (used to plug in for the specific positions)*)
"\[Theta]s="
\[Theta]s=ArcCos[(zp-z1)/Sqrt[(xp-x1s)^2+(yp-y1s)^2+(zp-z1)^2]];
"\[Phi]s="
\[Phi]s=ArcTan[xp-x1s,yp-y1s];

(*Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2bt"," "\[Alpha]2bt *180/\[Pi]}]]*)

(*y1s=y1Home
\[Alpha]2b;
\[Theta]b=\[Theta]s
\[Phi]b=\[Phi]s

Plot[\[Alpha]2b*180/\[Pi],{x1s,x1Home,x1Max}]*)



(*(*Test Values*)

"Home"
x1s=x1Home
y1s=y1Home

Print[Column[{"\[Degree]=\[Theta]s"," "\[Theta]s*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]s"," "\[Phi]s*180/\[Pi]}]]

\[Theta]b=\[Theta]s
\[Phi]b=\[Phi]s

"M2b-homeM2"
M2b-Mhome

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2bt"," "\[Alpha]2bt *180/\[Pi]}]]

(*Test Forward*)



\[Beta]1f=\[Beta]1b
\[Gamma]1f=\[Gamma]1b
\[Beta]2f=\[Beta]2b
\[Alpha]2f=\[Alpha]2b

"M2f-Mhome"
M2f-Mhome

Print[Column[{"\[Degree]=\[Theta]f"," "\[Theta]f*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]f"," "\[Phi]f*180/\[Pi]}]]*)


(*"YMax"
x1s=x1Home
y1s=y1Max

Print[Column[{"\[Degree]=\[Theta]"," "\[Theta]s*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]"," "\[Phi]s*180/\[Pi]}]]

\[Theta]b=\[Theta]s
\[Phi]b=\[Phi]s

"M2-homeM2"
M2b-Mhome

"M2-MyMax"
M2b-MyMax

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2bt"," "\[Alpha]2bt *180/\[Pi]}]]

(*Test Forward*)

\[Beta]1f=180\[Degree]-\[Beta]1b
\[Gamma]1f=\[Gamma]1b
\[Beta]2f=\[Beta]2b
\[Alpha]2f=\[Alpha]2b

"M2f-Mhome"
M2f-MyMax

Print[Column[{"\[Degree]=\[Theta]f"," "\[Theta]f*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]f"," "\[Phi]f*180/\[Pi]}]]*)


(*"XMax"
x1s=x1Max
y1s=y1Home

Print[Column[{"\[Degree]=\[Theta]"," "\[Theta]s*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]"," "\[Phi]s*180/\[Pi]}]]

\[Theta]b=\[Theta]s
\[Phi]b=\[Phi]s

"M2-homeM2"
M2b-Mhome

"M2-MxMax"
M2b-MxMax

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2bt"," "\[Alpha]2bt *180/\[Pi]}]]

(*Test Forward*)

\[Beta]1f=\[Beta]1b
\[Gamma]1f=\[Gamma]1b
\[Beta]2f=\[Beta]2b
\[Alpha]2f=\[Alpha]2b

"M2f-Mhome"
M2f-MxMax

Print[Column[{"\[Degree]=\[Theta]f"," "\[Theta]f*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]f"," "\[Phi]f*180/\[Pi]}]]*)


(*"XYMax"
x1s=x1Max
y1s=y1Max

Print[Column[{"\[Degree]=\[Theta]"," "\[Theta]s*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]"," "\[Phi]s*180/\[Pi]}]]

\[Theta]b=\[Theta]s
\[Phi]b=\[Phi]s

"M2-homeM2"
M2b-Mhome

"M2-MxyMax"
M2b-MxyMax

Print[Column[{"\[Degree]=\[Beta]1b"," "\[Beta]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1b"," "\[Gamma]1b *180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2b"," "\[Beta]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2b"," "\[Alpha]2b *180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2bt"," "\[Alpha]2bt *180/\[Pi]}]]

(*Test Forward*)

\[Beta]1f=\[Beta]1b
\[Gamma]1f=\[Gamma]1b
\[Beta]2f=\[Beta]2b
\[Alpha]2f=\[Alpha]2b

"M2f-Mhome"
M2f-MxyMax

Print[Column[{"\[Degree]=\[Theta]f"," "\[Theta]f*180/\[Pi]}],
Column[{"\[Degree]=\[Phi]f"," "\[Phi]f*180/\[Pi]}]]*)


(*Get error propagation for theta and phi. I have two eqs for these, thetaS and phiS, which 
are calculated from the M2 position, and thetaF and phiF, which depends on the mirror angles.
We find the error from thetaF and phiF,*)

\[Theta]f;
\[Phi]f;

\[Sigma]ThetaBeta1=D[\[Theta]f,\[Beta]1f];
\[Sigma]ThetaGamma1=D[\[Theta]f,\[Gamma]1f];
\[Sigma]ThetaBeta2=D[\[Theta]f,\[Beta]2f];
\[Sigma]ThetaAlpha2=D[\[Theta]f,\[Alpha]2f];

\[Sigma]PhiBeta1=D[\[Phi]f,\[Beta]1f];
\[Sigma]PhiGamma1=D[\[Phi]f,\[Gamma]1f];
\[Sigma]PhiBeta2=D[\[Phi]f,\[Beta]2f];
\[Sigma]PhiAlpha2=D[\[Phi]f,\[Alpha]2f];

\[Beta]1f=\[Beta]1b;
\[Gamma]1f=\[Gamma]1b;
\[Beta]2f=\[Beta]2b;
\[Alpha]2f=\[Alpha]2b;

DensityPlot[\[Sigma]ThetaBeta1^2/.\[Theta]b->q*\[Pi]/180/.\[Phi]b->u*\[Pi]/180,{q,0,85},{u,0,360},PlotPoints->{18,72},Mesh->Full,AxesLabel->{"\[Theta]","\[Phi]"},PlotLegends->Automatic]
DensityPlot[\[Sigma]ThetaGamma1^2/.\[Theta]b->q*\[Pi]/180/.\[Phi]b->u*\[Pi]/180,{q,0,85},{u,0,360},PlotPoints->{18,72},Mesh->Full,AxesLabel->{"\[Theta]","\[Phi]"},PlotLegends->Automatic]
DensityPlot[\[Sigma]ThetaBeta2^2/.\[Theta]b->q*\[Pi]/180/.\[Phi]b->u*\[Pi]/180,{q,0,85},{u,0,360},PlotPoints->{18,72},Mesh->Full,AxesLabel->{"\[Theta]","\[Phi]"},PlotLegends->Automatic]
DensityPlot[\[Sigma]ThetaAlpha2^2/.\[Theta]b->q*\[Pi]/180/.\[Phi]b->u*\[Pi]/180,{q,0,85},{u,0,360},PlotPoints->{18,72},Mesh->Full,AxesLabel->{"\[Theta]","\[Phi]"},PlotLegends->Automatic]

DensityPlot[\[Sigma]PhiBeta1^2/.\[Theta]b->q*\[Pi]/180/.\[Phi]b->u*\[Pi]/180,{q,0,85},{u,0,360},PlotPoints->{18,72},Mesh->Full,AxesLabel->{"\[Theta]","\[Phi]"},PlotLegends->Automatic]
DensityPlot[\[Sigma]PhiGamma1^2/.\[Theta]b->q*\[Pi]/180/.\[Phi]b->u*\[Pi]/180,{q,0,85},{u,0,360},PlotPoints->{18,72},Mesh->Full,AxesLabel->{"\[Theta]","\[Phi]"},PlotLegends->Automatic]
DensityPlot[\[Sigma]PhiBeta2^2/.\[Theta]b->q*\[Pi]/180/.\[Phi]b->u*\[Pi]/180,{q,0,85},{u,0,360},PlotPoints->{18,72},Mesh->Full,AxesLabel->{"\[Theta]","\[Phi]"},PlotLegends->Automatic]
DensityPlot[\[Sigma]PhiAlpha2^2/.\[Theta]b->q*\[Pi]/180/.\[Phi]b->u*\[Pi]/180,{q,0,85},{u,0,360},PlotPoints->{18,72},Mesh->Full,AxesLabel->{"\[Theta]","\[Phi]"},PlotLegends->Automatic]



