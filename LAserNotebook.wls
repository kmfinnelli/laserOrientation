#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

lin={
 {0},
 {-1},
 {0}
};
"lin:the incoming path of the laser [x,y,z]"
ny45={
 {0},
 {1},
 {0}
};
"ny45:the normal vector of the mirror"
rx[\[Alpha]_]:={
 {1, 0, 0},
 {0, Cos[\[Alpha]], -Sin[\[Alpha]]},
 {0, Sin[\[Alpha]], Cos[\[Alpha]]}
}
"rx: the rotation matrix around the x-axis; changes y position"

rz[\[Beta]_]:={
 {Cos[\[Beta]], -Sin[\[Beta]], 0},
 {Sin[\[Beta]], Cos[\[Beta]], 0},
 {0, 0, 1}
}

ry[\[Gamma]_]:={
 {Cos[\[Gamma]], 0, Sin[\[Gamma]]},
 {0, 1, 0},
 {-Sin[\[Gamma]], 0, Cos[\[Gamma]]}
}
"rz: the rotation matrix around the z-axis; changes x position "

rTot[\[Alpha]_,\[Beta]_]:=rz[\[Beta]].rx[\[Alpha]];
"rTot: The rotation matrix combination of changing both alpha and beta"

(*rAll[\[Alpha]_,\[Beta]_,\[Gamma]_]:=rz[\[Beta]].ry[\[Gamma]].rx[\[Alpha]]

\[Gamma]=0\[Degree]*)

MatrixForm[rTot[\[Alpha],\[Beta]]];

(*MatrixForm[rAll[\[Alpha],\[Beta],\[Gamma]]]*)


"Alpha Only"

nyAlpha[\[Alpha]_]:=rx[\[Alpha]].ny45;
"nyAlpha: the normal vector after the mirror has been rotated around the x-axis by an angle alpha, with -45<alpha<45"
(*nyAlpha[\[Alpha]]
"nyAlpha[15 \[Degree]]"
Simplify[nyAlpha[15\[Degree]]]*)

dotlnAlpha[\[Alpha]_]:=Transpose[nyAlpha[\[Alpha]]].lin;
(*"dotlnAlpha[\[Alpha]_]"
dotlnAlpha[\[Alpha]]
"dotlnAlpha[15]"
dotlnAlpha[15\[Degree]]*)

"loutAlpha: the outgoing path of the laser when the mirror was rotated by alpha [x,y,z]"
loutAlpha[\[Alpha]_]:=lin-2*(dotlnAlpha[\[Alpha]][[1,1]])*nyAlpha[\[Alpha]];

(*Simplify[loutAlpha[\[Alpha]]]

Style[Simplify[loutAlpha[45\[Degree]]],FontColor\[Rule]Red]*)





"Beta Only"

"nxBeta: the normal vector after the mirror has been rotatated around the z-axis by an angle Beta, "
nxBeta[\[Beta]_]:=rz[\[Beta]].ny45;
(*"nxBeta"
nxBeta[\[Beta]]*)

dotlnBeta[\[Beta]_]:=Transpose[nxBeta[\[Beta]]].lin;
(*"dotlnBeta[\[Beta]_]"
dotlnBeta[\[Beta]]*)

"loutBeta: the outgoing path of the laser when the mirror was rotated by beta [x,y,z]"
loutBeta[\[Beta]_]:=lin-2*(dotlnBeta[\[Beta]][[1,1]])*nxBeta[\[Beta]];

(*Simplify[loutBeta[\[Beta]]]

Style[Simplify[loutBeta[-45\[Degree]]],FontColor\[Rule]Blue]*)



"C-C-C-C-COMBO"
nBoth[\[Alpha]_,\[Beta]_]:=rTot[\[Alpha],\[Beta]].ny45;
(*"nBoth[\[Alpha]_,\[Beta]_]"
nBoth[\[Alpha],\[Beta]]*)

dotlnBoth[\[Alpha]_,\[Beta]_]:=Transpose[nBoth[\[Alpha],\[Beta]]].lin;
(*"dotlnBoth[\[Alpha]_,\[Beta]_]"
dotlnBoth[\[Alpha],\[Beta]]*)

"loutBoth: the outgoing path of the laser when the mirror was rotated by alpha and beta [x,y,z]"
loutBoth[\[Alpha]_,\[Beta]_]:=lin-2*(dotlnBoth[\[Alpha],\[Beta]][[1,1]])*nBoth[\[Alpha],\[Beta]];

Simplify[loutBoth[\[Alpha],\[Beta]]]

Style[Simplify[loutBoth[45\[Degree],0\[Degree]]],FontColor->Purple]

"We now have the path of the laser when it is reflected off a mirror that has been rotated from a 
mirror that is 45 deg by an angle alpha around the x axis and beta around the z axis. Now we find 
where it lands at a final z position, zp"

z1[\[Alpha]_,\[Beta]_]:=Simplify[loutBoth[\[Alpha],\[Beta]][[3,1]]]
"z1[\[Alpha],\[Beta]]"
z1[\[Alpha],\[Beta]];

zAdj[\[Alpha]_,\[Beta]_,zp_]:=zp/z1[\[Alpha],\[Beta]]
"zAdj[\[Alpha],\[Beta],zp]"
zAdj[\[Alpha],\[Beta],zp];

loutzp[\[Alpha]_,\[Beta]_,zp_]:=loutBoth[\[Alpha],\[Beta]]*zAdj[\[Alpha],\[Beta],zp]
"loutzp[\[Alpha],\[Beta],zp]"
Style[Simplify[loutzp[\[Alpha],\[Beta],zp]],FontColor-> Red];

"HERE"

"Because this is trying to find where the laser lands at vertical position zp, it will fail when 
alpha=0 because the laser will have no vertical change to get up to zp"
Style[Simplify[loutzp[45\[Degree],45\[Degree],zp]],FontColor-> CMYKColor[1,.1,0,.5]];



"Get out how alpha and beta can affect the postion as x and y prime"

xPrime[\[Alpha]_,\[Beta]_,zp_]:=Simplify[loutzp[\[Alpha],\[Beta],zp][[1,1]]]
(*"xPrime[\[Alpha],\[Beta],zp]"
xPrime[\[Alpha],\[Beta],zp]*)

yPrime[\[Alpha]_,\[Beta]_,zp_]:=Simplify[loutzp[\[Alpha],\[Beta],zp][[2,1]]]
(*"yPrime[\[Alpha],\[Beta],zp]"
yPrime[\[Alpha],\[Beta],zp]*)

"We now have the x and y offsets due to the rotation of the mirror. With this, we can get 
the total offset from the position of the beam pipe, [xp,yp,zp]. This is of the form 
deltaX=xi+xPrime-xp
deltaY=yi+yPrime-yp
Where [xi,yi,zi] is the position of the final mirror where the laser hits"

deltaX[\[Alpha]_,\[Beta]_,zp_,xi_,xp_]:=xi+xPrime[\[Alpha],\[Beta],zp]-xp
"deltaX[\[Alpha],\[Beta],zp,xi,xp]"
deltaX[\[Alpha],\[Beta],zp,xi,xp];

deltaY[\[Alpha]_,\[Beta]_,zp_,yi_,yp_]:=yi+yPrime[\[Alpha],\[Beta],zp]-yp
"deltaY[\[Alpha],\[Beta],zp,yi,yp]"
deltaY[\[Alpha],\[Beta],zp,yi,yp];

"Partials of deltaX"

ddeltaXxp[\[Alpha]_,\[Beta]_,zp_,xi_,xp_]:=D[deltaX[\[Alpha],\[Beta],zp,xi,xp],xp]
(*"ddeltaXxp"
ddeltaXxp[\[Alpha],\[Beta],zp,xi,xp]*)

ddeltaXxi[\[Alpha]_,\[Beta]_,zp_,xi_,xp_]:=D[deltaX[\[Alpha],\[Beta],zp,xi,xp],xi]
(*"ddeltaXxi"
ddeltaXxi[\[Alpha],\[Beta],zp,xi,xp]*)

ddeltaXzp[\[Alpha]_,\[Beta]_,zp_,xi_,xp_]:=D[deltaX[\[Alpha],\[Beta],zp,xi,xp],zp]
(*"ddeltaXzp"
ddeltaXzp[\[Alpha],\[Beta],zp,xi,xp]*)

ddeltaXbeta[\[Alpha]_,\[Beta]_,zp_,xi_,xp_]:=D[deltaX[\[Alpha],\[Beta],zp,xi,xp],\[Beta]]
(*"ddeltaXbeta"
ddeltaXbeta[\[Alpha],\[Beta],zp,xi,xp]*)

ddeltaXalpha[\[Alpha]_,\[Beta]_,zp_,xi_,xp_]:=D[deltaX[\[Alpha],\[Beta],zp,xi,xp],\[Alpha]]
(*"ddeltaXalpha"
ddeltaXalpha[\[Alpha],\[Beta],zp,xi,xp]*)

"Partials of deltaY"

ddeltaYyp[\[Alpha]_,\[Beta]_,zp_,yi_,yp_]:=D[deltaY[\[Alpha],\[Beta],zp,yi,yp],yp]
(*"ddeltaYyp"
ddeltaYyp[\[Alpha],\[Beta],zp,xi,xp]*)

ddeltaYyi[\[Alpha]_,\[Beta]_,zp_,yi_,yp_]:=D[deltaY[\[Alpha],\[Beta],zp,yi,yp],yi]
(*"ddeltaYyi"
ddeltaYyi[\[Alpha],\[Beta],zp,xi,xp]*)

ddeltaYzp[\[Alpha]_,\[Beta]_,zp_,yi_,yp_]:=D[deltaY[\[Alpha],\[Beta],zp,yi,yp],zp]
(*"ddeltaYzp"
ddeltaYzp[\[Alpha],\[Beta],zp,xi,xp]*)

ddeltaYbeta[\[Alpha]_,\[Beta]_,zp_,yi_,yp_]:=D[deltaY[\[Alpha],\[Beta],zp,yi,yp],\[Beta]]
(*"ddeltaYbeta"
ddeltaYbeta[\[Alpha],\[Beta],zp,xi,xp]*)

ddeltaYalpha[\[Alpha]_,\[Beta]_,zp_,yi_,yp_]:=D[deltaY[\[Alpha],\[Beta],zp,yi,yp],\[Alpha]]
(*"ddeltaYalpha"
ddeltaYalpha[\[Alpha],\[Beta],zp,xi,xp]*) 


"These are set as = not := because constants cannot be differentiated"

sigmadeltaX[\[Alpha]_,\[Beta]_,zp_,xi_,xp_]=((ddeltaXxp[\[Alpha],\[Beta],zp,xi,xp])^2*\[Sigma]xp^2+(ddeltaXxi[\[Alpha],\[Beta],zp,xi,xp])^2*\[Sigma]xi^2+
				(ddeltaXzp[\[Alpha],\[Beta],zp,xi,xp])^2*\[Sigma]zp^2+(ddeltaXbeta[\[Alpha],\[Beta],zp,xi,xp])^2*\[Sigma]Beta^2+(ddeltaXalpha[\[Alpha],\[Beta],zp,xi,xp])^2*\[Sigma]Alpha^2)^(1/2);

sigmadeltaY[\[Alpha]_,\[Beta]_,zp_,xi_,xp_]=((ddeltaYyp[\[Alpha],\[Beta],zp,xi,xp])^2*\[Sigma]yp^2+(ddeltaYyi[\[Alpha],\[Beta],zp,xi,xp])^2*\[Sigma]yi^2+
				(ddeltaYzp[\[Alpha],\[Beta],zp,xi,xp])^2*\[Sigma]zp^2+(ddeltaYbeta[\[Alpha],\[Beta],zp,xi,xp])^2*\[Sigma]Beta^2+(ddeltaYalpha[\[Alpha],\[Beta],zp,xi,xp])^2*\[Sigma]Alpha^2)^(1/2);
	
			
"Simplify[sigmadeltaX[\[Alpha],\[Beta],zp,xi,xp]]"
Simplify[sigmadeltaX[\[Alpha],\[Beta],zp,xi,xp]];

"Simplify[sigmadeltaY[\[Alpha],\[Beta],zp,xi,xp]]"
Simplify[sigmadeltaY[\[Alpha],\[Beta],zp,xi,xp]];








"Turn equations for delta x/y to xi/yi, to get equations for position in terms of angles."

"Simplify[deltaX[\[Alpha],\[Beta],zp,xi,xp]]"
Simplify[deltaX[\[Alpha],\[Beta],zp,xi,xp]];
"Simplify[Solve[deltaX[\[Alpha],\[Beta],zp,xi,xp]\[Equal]0,xi]]"
"xiFunc"
xiFunc=Simplify[Solve[deltaX[\[Alpha],\[Beta],zp,xi,xp]==0,xi]];


"Simplify[deltaY[\[Alpha],\[Beta],zp,yi,yp]]"
Simplify[deltaY[\[Alpha],\[Beta],zp,yi,yp]];
"Simplify[Solve[deltaY[\[Alpha],\[Beta],zp,yi,yp]==0,yi]]"
"yiFunc"
yiFunc=Simplify[Solve[deltaY[\[Alpha],\[Beta],zp,yi,yp]==0,yi]];





(*
"Attempts to get alpha and beta as functions of positions, still extremely long, output is saved in notebook FuncAnglesAsPositions"
"Solve[deltaX[\[Alpha],\[Beta],zp,xi,xp]\[Equal]0&&deltaY[\[Alpha],\[Beta],zp,yi,yp]==0,{\[Alpha],\[Beta]}]"
Solve[deltaX[\[Alpha],\[Beta],zp,xi,xp]\[Equal]0&&deltaY[\[Alpha],\[Beta],zp,yi,yp]==0,{\[Alpha],\[Beta]}]
Simplify[Solve[deltaX[\[Alpha],\[Beta],zp,xi,xp]\[Equal]0&&deltaY[\[Alpha],\[Beta],zp,yi,yp]==0,{\[Alpha],\[Beta]}]]
*)

(* Hardcopied Verion
Simplify[Solve[xi-xp-(zp (Cos[\[Alpha]]-Sin[\[Alpha]]) Sin[\[Beta]])/(Cos[\[Alpha]]+Sin[\[Alpha]])==0,xi]]
(*Solve[deltaX[\[Alpha],\[Beta],zp,xi,xp]==0,\[Alpha]]
Solve[deltaX[\[Alpha],\[Beta],zp,xi,xp]=0,\[Beta]]*)

Simplify[deltaY[\[Alpha],\[Beta],zp,yi,yp]]
Simplify[Solve[yi-yp-zp Cos[\[Beta]] Sec[2 \[Alpha]] (-Cos[\[Alpha]]^2+Sec[\[Beta]]^2-Sin[\[Alpha]]^2+Sin[2 \[Alpha]])==0,yi]]
(*Solve[deltaY[\[Alpha],\[Beta],zp,yi,yp]=0,\[Alpha]]
Solve[deltaY[\[Alpha],\[Beta],zp,yi,yp]=0,\[Beta]]*)*)

(*Simplify[Solve[(xi-xp-(zp (Cos[\[Alpha]]-Sin[\[Alpha]]) Sin[\[Beta]])/(Cos[\[Alpha]]+Sin[\[Alpha]]))==0&&(xi-xp-zp Cos[\[Beta]] Sec[2 \[Alpha]] 
		(-Cos[\[Alpha]]^2+Sec[\[Beta]]^2-Sin[\[Alpha]]^2+Sin[2 \[Alpha]]))==0,{\[Alpha],\[Beta]}]]*)











"Get partials of xi/yi to get error propagation of mirror position with respect to angles"

"dxis"
"alpha"
dxiAlpha=D[xiFunc,\[Alpha]];
"xp"
dxixp=D[xiFunc,xp];
"zp"
dxizp=D[xiFunc,zp];
"beta"
dxiBeta=D[xiFunc,\[Beta]];

"dyis"
"alpha"
dyiAlpha=D[yiFunc,\[Alpha]];
"yp"
dyiyp=D[yiFunc,yp];
"zp"
dyizp=D[yiFunc,zp];
"beta"
dyiBeta=D[yiFunc,\[Beta]];

"sigma xiS"
\[Sigma]xiS=Simplify[(dxiAlpha^2*\[Sigma]Alpha^2+dxixp^2*\[Sigma]xp^2+dxizp^2*\[Sigma]zp^2+dxiBeta^2*\[Sigma]Beta^2)^(1/2)];
"sigmaa yiS"
\[Sigma]yiS=Simplify[(dyiAlpha^2*\[Sigma]Alpha^2+dyiyp^2*\[Sigma]yp^2+dyizp^2*\[Sigma]zp^2+dyiBeta^2*\[Sigma]Beta^2)^(1/2)];

"Error Propagations of xi and yi, HARDCOPIED IN, change result if above changes"
"\[Sigma]xi"
\[Sigma]xi=\[Sqrt](\[Sigma]xp^2 (1)^2+\[Sigma]Beta^2 (zp Cos[\[Beta]] Cot[\[Alpha]])^2+\[Sigma]zp^2 (Cot[\[Alpha]] Sin[\[Beta]])^2+\[Sigma]Alpha^2 (-zp Csc[\[Alpha]]^2 Sin[\[Beta]])^2)
"\[Sigma]yi"
\[Sigma]yi=\[Sqrt](\[Sigma]yp^2 (1)^2+\[Sigma]zp^2 (-Cos[\[Beta]] Cot[\[Alpha]]+Csc[2 \[Alpha]] Sec[\[Beta]])^2+\[Sigma]Alpha^2 (zp Cos[\[Beta]] Csc[\[Alpha]]^2-2 zp Cot[2 \[Alpha]] Csc[2 \[Alpha]] 
			Sec[\[Beta]])^2+\[Sigma]Beta^2 (zp (Cot[\[Alpha]] Sin[\[Beta]]+Csc[2 \[Alpha]] Sec[\[Beta]] Tan[\[Beta]]))^2)



Remove[\[Theta]]
Remove[\[Phi]]

\[Beta]=-0


"In TPC Coordinates, converting "
\[Theta]=ArcCos[-Cos[\[Alpha]](Cos[\[Beta]])^2]
\[Phi]=ArcCot[(-2Cos[\[Beta]]Sin[\[Beta]](Cos[\[Alpha]])^2)/(-1+2(Cos[\[Alpha]])^2(Cos[\[Beta]])^2)]
\[Phi]1=ArcCos[-2Cos[\[Beta]]Csc[\[Theta]]Sin[\[Beta]](Cos[\[Alpha]])^2]
\[Phi]2=ArcSin[Csc[\[Theta]](-1+2(Cos[\[Alpha]])^2(Cos[\[Beta]])^2)]

Plot[{\[Theta],\[Phi],\[Phi]1,\[Phi]2},{\[Alpha],0,\[Pi]/2},PlotLegends->"Expressions"]

(*Remove[\[Sigma]Alpha]*)

(*\[Alpha]=ArcTan[(zp*Sin[\[Beta]])/(xi-xp)]

"\[Sigma]Alphazp"
\[Sigma]Alphazp=D[\[Alpha],zp]
"\[Sigma]Alphaxi"
\[Sigma]Alphaxi=D[\[Alpha],xi]
"\[Sigma]Alphaxp"
\[Sigma]Alphaxp=D[\[Alpha],xp]
"\[Sigma]AlphaBeta"
\[Sigma]AlphaBeta=D[\[Alpha],\[Beta]]

\[Sigma]Alpha=\[Sqrt](\[Sigma]zp^2\[Sigma]Alphazp^2+\[Sigma]xi^2\[Sigma]Alphaxi^2+\[Sigma]xp^2\[Sigma]Alphaxp^2+\[Sigma]Beta^2\[Sigma]AlphaBeta^2)*)







