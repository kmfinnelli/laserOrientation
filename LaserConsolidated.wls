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

(*Position of beam pipe*)
xp=4.87 ;
yp=8.13 ;
zp=4.19 ;


(*Going backward*)

(* Position of mirror 2 in terms of theta and phi in the TPC*)

"xi=" 
x1[\[Theta]_,\[Phi]_]:=xp-((z1-z0)/Cos[\[Theta]])*Sin[\[Theta]]Cos[\[Phi]];
"yi=" 
y1[\[Theta]_,\[Phi]_]:=yp-((z1-z0)/Cos[\[Theta]])*Sin[\[Theta]]Sin[\[Phi]];

(*Vector Coordinates of mirror 2 in terms of theta and phi*)
"M2=" 
M2={x1[\[Theta],\[Phi]],y1[\[Theta],\[Phi]],z1};

(*Vector Coordinates of mirror 1; constant*)
"M1=" 
M1={x0,y0,z0};

(*Path of the laser that goes from the center of mirror 1 and center of mirror 2, 
determined by geometry*)
"lout1T=" 
lout1T=(M2-M1)/Sqrt[(M2-M1).(M2-M1)];

(* 
This method gives the same result as above
lout1T2={x1[\[Theta],\[Phi]]-x0,y1[\[Theta],\[Phi]]-y0,z1-z0}
"lout1T2"
lout1T32=Normalize[lout1T2]
*)



(*
Commented out to save time
(*Path of the laser that goes from the center of mirror 2 to the position of the beam pipe. It
is described in terms of theta and phi, because those determine the path of the beam going 
into the pipe, which must be what comes out of mirror 2*)*)
"lout2T=" 
lout2T={Sin[\[Theta]]Cos[\[Phi]],Sin[\[Theta]]Sin[\[Phi]],Cos[\[Theta]]};

(*
(*The normal vector of mirror 2 can be determined by using these two input and output 
vectors *)
"n2=" 
n2=(-lout1T+lout2T)/Sqrt[2-2lout1T.lout2T]
n2[[3]];

(*The normal vector can also be found in terms of the angles that the mirror can rotate. 
Mirror 2 can rotate along 2 axis, along the x-axis by angle \[Alpha]2 and around the z-axis by
\[Beta]2. By applying the rotation matrices of both these transformations to the unrotated 
normal vector of the mirror, we can find the mirror when it has been rotated in terms of 
the two angles. The original normal vector of mirror 2 is (0,-1,0), because it points in
the negative y-axis*)*)

rx2=RotationTransform[\[Alpha]2,{1,0,0}];
rz2=RotationTransform[\[Beta]2,{0,0,1}];

"nAngle2="
nAngle2=rz2[rx2[{0,-1,0}]]

(*We now have descriptions of the normal vector in terms of both theta and phi and alpha2 
and beta2. We can set these equal to each other and find a way to decribe the mirror angles
in terms of the TPC angles. *)

"\[Alpha]2="
(*Simplify[Solve[n2[[3]]==nAngle2[[3]],{\[Alpha]2}]];*)

(*\[Alpha]2=ArcSin[-((Cos[\[Theta]] + (0.2179 in)/Sqrt[0.0474804 in^2 + (2.34252 in + 
      0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2])/(\[Sqrt](2 - 
     2 (-((0.2179 in Cos[\[Theta]])/(\[Sqrt](0.0474804 in^2 + (2.34252 in + 0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 
     + (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2))) + (Cos[\[Phi]] Sin[\[Theta]] (2.34252 in + 
     0.2179 in Cos[\[Phi]] Tan[\[Theta]]))/(\[Sqrt](0.0474804 in^2 + (2.34252 in + 
     0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2))+
      (Sin[\[Theta]] Sin[\[Phi]] (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]]))/(\[Sqrt](0.0474804 in^2 + (2.34252 in + 
       0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 in +  0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2))))))];*)
       
"\[Beta]2="
(*Simplify[Solve[n2[[1]]/n2[[2]]==nAngle2[[1]]/nAngle2[[2]],{\[Beta]2}]]*)

(*\[Beta]2=ArcTan[(Sin[\[Theta]] Sin[\[Phi]] - (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])/Sqrt[0.0474804 in^2 + (2.34 in + 
      0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2])/(\[Sqrt](2 - 
     2 (-((0.2179 in Cos[\[Theta]])/(\[Sqrt](0.0474804 in^2 + (2.34 in + 0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 + 
     (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2))) + (Cos[\[Phi]] Sin[\[Theta]] (2.34 in + 
      0.2179 in Cos[\[Phi]] Tan[\[Theta]]))/(\[Sqrt](0.0474804 in^2 + (2.34 in + 0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 +
     (7.1 in +  0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2)) + (Sin[\[Theta]] Sin[\[Phi]] (7.1 in + 
      0.2179 in Sin[\[Phi]] Tan[\[Theta]]))/(\[Sqrt](0.0474804 in^2 + (2.34 in + 0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 +
       (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2))))),
       -(Cos[\[Phi]] Sin[\[Theta]] - (2.34 in + 0.2179 in Cos[\[Phi]] Tan[\[Theta]])/Sqrt[0.0474804 in^2 + 
	(2.34 in + 0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2])/(\[Sqrt](2 - 
     2 (-((0.2179 in Cos[\[Theta]])/(\[Sqrt](0.0474804 in^2 + (2.34 in + 0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 + 
     (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2))) + (Cos[\[Phi]] Sin[\[Theta]] (2.34 in + 
     0.2179 in Cos[\[Phi]] Tan[\[Theta]]))/(\[Sqrt](0.0474804 in^2 + (2.34 in + 0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 +
      (7.1 in +  0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2)) + (Sin[\[Theta]] Sin[\[Phi]] (7.1 in + 
     0.2179 in Sin[\[Phi]] Tan[\[Theta]]))/(\[Sqrt](0.0474804 in^2 + (2.34 in + 0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 +
      (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2)))))];*)


(*(* 
Commented out to save time
Now for mirror 1; we already have the path of the laser going between mirror 1 and mirror
2 as lout1T. The incoming laser going into mirror 1 is given as (-1,0,0) because it is 
heading in the negative x-direction.*)*)

lout0T={-1,0,0};

(*
(* Then the normal vector of mirror 1 between these vectors is*)
"n1="
n1=(-lout0T+lout1T)/Sqrt[2-2lout0T.lout1T];*)

(* Again, we can find this normal vector in therms of the mirror 1 angles. It rotates around
the y-axis by angle \[Gamma]1 and around the z-axis by angle \[Beta]1*)
ry1=RotationTransform[\[Gamma]1,{0,1,0}];
rz1=RotationTransform[\[Beta]1,{0,0,1}];

"nAngle1="
nAngle1=rz1[ry1[{1,0,0}]];

"\[Gamma]1="
(*Simplify[n1[[3]]\[Equal]nAngle2[[3]],{\[Gamma]1}]*)

(*\[Gamma]1=-((0.2179 in)/(Sqrt[ 0.0474804 in^2 + (2.34 in +  0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 + 
	(7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2] \[Sqrt](2 - 2 (0. - (2.34 in + 
	0.2179 in Cos[\[Phi]] Tan[\[Theta]])/(\[Sqrt](0.0474804 in^2 + (2.34 in + 0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 + 
	(7.1 in +  0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2))))));*)

"\[Beta]1="
(*Simplify[Solve[n1[[1]]/n1[[2]]==nAngle1[[1]]/nAngle1[[2]],{\[Beta]1}]]*)

(*\[Beta]1=ArcTan[(1 + (2.34 in + 0.2179 in Cos[\[Phi]] Tan[\[Theta]])/Sqrt[0.0474804 in^2 + (2.34 in + 
      0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2])/Sqrt[2 - 
      2 (0. - (2.34 in + 0.2179 in Cos[\[Phi]] Tan[\[Theta]])/Sqrt[0.0474804 in^2 + (2.34 in + 
       0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2])],
       (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])/(Sqrt[0.0474804 in^2 + (2.34 in + 
      0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2] \[Sqrt](2 - 
      2 (0. - (2.34 in + .2179 in Cos[\[Phi]] Tan[\[Theta]])/(\[Sqrt](0.0474804 in^2 + 
     (2.34 in + 0.2179 in Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 in + 0.2179 in Sin[\[Phi]] Tan[\[Theta]])^2)))))];*)

(*Get \[Gamma]1 and \[Beta]1*)





(*NOW: Go Forward*)

(*Initial laser vector going to Mirrror 1 is lout0T*)

(*Original normal vector of mirror 1 is n1*)

n1F={1,0,0};

(*Same output as nAngle1*)

"dotlnBoth1="
dotlnBoth1=nAngle1.lout0T
"loutBoth1="
loutBoth1=lout0T-2*dotlnBoth1*nAngle1

"loutBoth1="
Style[Simplify[loutBoth1],FontColor->Purple]

(* This output goes into the second mirror *)

n2F={0,-1,0};

(*Same output as nAngle2*)
dotlnBoth2=nAngle2.loutBoth1;

loutBoth2=loutBoth1-2*dotlnBoth2*nAngle2;

" The laser leaving mirror 2, loutBoth2="
loutBoth2=Style[Simplify[loutBoth2],FontColor->Blue]

(* Extract the x, y, and z corrdinates from this *)

xF=loutBoth2[[1,1]];
yF=loutBoth2[[1,2]];
zF=loutBoth2[[1,3]];

(* In TPC Corrdinates *)

(*\[Theta]=ArcCos[zF];
\[Theta]=Simplify[\[Theta]];
\[Phi]=ArcTan[xF,yF];
\[Phi]=Simplify[\[Phi]];*)

(* Eqs for angles *)

\[Gamma]1=-((0.2179 )/(Sqrt[ 0.0474804^2 + (2.34  +  0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 + 
	(7.1  + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2] \[Sqrt](2 - 2 (0. - (2.34 + 
	0.2179 Cos[\[Phi]] Tan[\[Theta]])/(\[Sqrt](0.0474804^2 + (2.34 + 0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 + 
	(7.1 +  0.2179 Sin[\[Phi]] Tan[\[Theta]])^2))))));
	
\[Beta]1=ArcTan[(1 + (2.34 + 0.2179 Cos[\[Phi]] Tan[\[Theta]])/Sqrt[0.0474804^2 + (2.34 + 
      0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2])/Sqrt[2 - 
      2 (0. - (2.34 + 0.2179 Cos[\[Phi]] Tan[\[Theta]])/Sqrt[0.0474804^2 + (2.34 + 
       0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2])],
       (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])/(Sqrt[0.0474804^2 + (2.34 + 
      0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2] \[Sqrt](2 - 
      2 (0. - (2.34 + .2179 Cos[\[Phi]] Tan[\[Theta]])/(\[Sqrt](0.0474804^2 + 
     (2.34 + 0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2)))))];
     
\[Alpha]2=ArcSin[-((Cos[\[Theta]] + (0.2179)/Sqrt[0.0474804^2 + (2.34252 + 
      0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2])/(\[Sqrt](2 - 
     2 (-((0.2179 Cos[\[Theta]])/(\[Sqrt](0.0474804^2 + (2.34252 + 0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 
     + (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2))) + (Cos[\[Phi]] Sin[\[Theta]] (2.34252 + 
     0.2179 Cos[\[Phi]] Tan[\[Theta]]))/(\[Sqrt](0.0474804^2 + (2.34252 + 
     0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2))+
      (Sin[\[Theta]] Sin[\[Phi]] (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]]))/(\[Sqrt](0.0474804^2 + (2.34252 + 
       0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 +  0.2179 Sin[\[Phi]] Tan[\[Theta]])^2))))))];
       
\[Beta]2=ArcTan[(Sin[\[Theta]] Sin[\[Phi]] - (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])/Sqrt[0.0474804^2 + (2.34 + 
      0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2])/(\[Sqrt](2 - 
     2 (-((0.2179 Cos[\[Theta]])/(\[Sqrt](0.0474804^2 + (2.34 + 0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 + 
     (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2))) + (Cos[\[Phi]] Sin[\[Theta]] (2.34 + 
      0.2179 Cos[\[Phi]] Tan[\[Theta]]))/(\[Sqrt](0.0474804^2 + (2.34 + 0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 +
     (7.1 +  0.2179 Sin[\[Phi]] Tan[\[Theta]])^2)) + (Sin[\[Theta]] Sin[\[Phi]] (7.1 + 
      0.2179 Sin[\[Phi]] Tan[\[Theta]]))/(\[Sqrt](0.0474804^2 + (2.34 + 0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 +
       (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2))))),
       -(Cos[\[Phi]] Sin[\[Theta]] - (2.34 + 0.2179 Cos[\[Phi]] Tan[\[Theta]])/Sqrt[0.0474804^2 + 
	(2.34 + 0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 + (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2])/(\[Sqrt](2 - 
     2 (-((0.2179 Cos[\[Theta]])/(\[Sqrt](0.0474804^2 + (2.34 + 0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 + 
     (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2))) + (Cos[\[Phi]] Sin[\[Theta]] (2.34 + 
     0.2179 Cos[\[Phi]] Tan[\[Theta]]))/(\[Sqrt](0.0474804^2 + (2.34 + 0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 +
      (7.1 +  0.2179 Sin[\[Phi]] Tan[\[Theta]])^2)) + (Sin[\[Theta]] Sin[\[Phi]] (7.1 + 
     0.2179 Sin[\[Phi]] Tan[\[Theta]]))/(\[Sqrt](0.0474804^2 + (2.34 + 0.2179 Cos[\[Phi]] Tan[\[Theta]])^2 +
      (7.1 + 0.2179 Sin[\[Phi]] Tan[\[Theta]])^2)))))];




"yMax"
"\[Theta]="
\[Theta]=(90\[Degree]-5.27\[Degree])*180/\[Pi]
"\[Phi]="
\[Phi]=ArcTan[x1Home-xp,y1Max-yp]*180/\[Pi]


"\[Alpha]2=" 
\[Alpha]2 *180/\[Pi]
"\[Beta]2=" 
\[Beta]2 *180/\[Pi]
"\[Gamma]1="
\[Gamma]1 *180/\[Pi]
"\[Beta]1="
\[Beta]1 *180/\[Pi]

(*For certain*)






