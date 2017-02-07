(* ::Package:: *)

(** :: Package :: **)
(* Tilings V-0.1, 07/02/2017 *)
(* Authors: Chuang Sun, Yan Xiao *)

(* Errors and Warnings *)
BeginPackage["Tilings`"]
Tilings::"Error"="This is an error for the testing of the package.";
(* This is the space for help information *)

Begin["`Private`"]
debugFlag=True;
(* Define our own version of error print function. Deal with the errors. *)
End[]

(* Begin writing usages. *)
(* Modules. *)
DPrint::usage="A print function for debugging.";
debugFlag::usage="Flag for DPrint[]";
Tilings::usage=
"Tilings Package V-0.1, 07/02/2017
\nContributed by authors in ArXiv:..
\nThis is a computational package of bipartite field theory. Given any theory with the SL(2,\[DoubleStruckCapitalZ]) reduced 2-d toric diagram, the package is able to compute its corresponding dimer model, super-potential terms and quiver graph. 
\nThe package is brounght in along with the paper: XXXXXXXX TO BE FILLED IN XXXXXXX
\nThe Tilings contains the following modules:
________________________
1) RecDimerModels[]
2) ToricInfo[]
3) DrawPerfectMatching[]
4) TriangDimer[]
5) RemovePointsParallel[]
6) HiggsingDimerSU[]
________________________
Use ?NameOfModule to view the details of each module.
Use DimerDemo[] to view a demonstration of dP3 surface.";
RecDimerModels::usage=
"Initialise the rectangular dimer model.
________________________
RecDimerModels[m,n] will returns the initialised dimer model 
as the Z_m x Z_n quotient of a conifold. 
The output is in the form 
{DimerGraph -> # graph representation of the m x n dimer model #, KMatrix-> # the Kasteleyn matrix #}.";
ToricInfo::usage="Compute the perfect matchings and toric diagram given a Kasteleyn matrix.
________________________
ToricInfo[KM,_QPrintFormat] will output the following list:
{KMatrix -> # the K matrix (same as input) #, PMatrix -> # the perfect matchings in a matrix form, where 1 stands for the presence of a field in a perfect matching. #, PerfectMatchings -> # a readable form of perfect matchings, denoting all the fields X_{i,j} #, ToricDiagram -> # the toric digram #, ToricPerfMap -> # denoting the correspondence between toric points and perfect matchings. #, FieldsPerfInfo -> # the counting of which perfect matchings contain a certain field #, ToricPts -> # the list of toric points. #}
If the user set the default parameter QPrintFormat as True, then the module will firstly print out a readable form of all the above items.";
DrawPerfectMatching::usage="Highlight a perfect matching in the brane tiling.
________________________
DrawPerfectMatching[APerfectMatching] will ouput a graphical representation of the input perfect matching, highlighted in dotted green lines. ";
TriangDimer::usage="Triangulation of a dimer model, with some fields deleted.
________________________
TriangDimer[KM,# fields to remove #] will output the triangulated toric diagram, if certain fields were removed from a dimer with Kasteleyn matrix KM.";
RemovePointsParallel::usage="Compute the partial resolutions given the Kasteleyn matrix and points to remove.
________________________
RemovePointsParallel[KM, # points to remove from a toric diagram #] will start all kernels and compute all the possible partial resolutions if we want to remove certain points from a dimer model with Kasteleyn matrix KM.";Remo
HiggsingDimerSU::usage="Compute the dimer model of the target geometry.
________________________
HiggsingDimerSU[# fields to remove #] will output the following result: {HiggsingDimer -> # graphical object representing the new dimer model #, SUW -> # the super-potential terms of the new dimer model #, Quiver -> # the new quiver graph #, Content -> # summary of all field contents #}
";
DimerDemo::usage="A demonstration of modules.";
(* Keywords. *)
PMatrix::usage="Keyword of perfect matchings in a matrix form.";
PerfectMatchings::usage="Keyword of perfect matchings, in the form of product of Xs";
ToricDiagram::usage="Keyword of the toric diagram of the rectangular dimer.";
ToricPerfMap::usage="Keyword of a map from toric points to the corresponding perfect matchings";
FieldsPerfInfo::usage="Keyword of a map showing how does a certain field appear in different perfect matchings.";
ToricPts::usage="Keyword of a list containing toric points.";
RemoveAntz::usage="Keyword of the list containing fields/edges to remove in a brane tiling";
NewToricTiang::usage="Keyword of triangulated toric diagram after certain edges removed from brane tiling.";
NewToricDiag::usage="Keyword of new toric diagram.";
DimerArea::usage="Keyword of area of a dimer model.";
HiggsingDimer::usage="Keyword of the new dimer.";
SUW::usage="Keyword of super-potential terms.";
Quiver::usage="Keyword of quiver graph of the new dimer.";
QuiverS::usage="Keyword of quiver graph of the new dimer re-labelled.";
ZZPaths::usage="Keyword of zig-zag paths.";
Content::usage="Keyword of the field contents.";
Maps::usage="Keyword of the map from old index to new index.";
ConQ::usage="Keyword of consistency check.";
(* Global variables. *)
X::usage="Variables for fields.";
x::usage="Vertical axis of unit cell.";
y::usage="Horizontal axis of unit cell.";
b::usage="Black nodes in dimer.";
w::usage="White nodes in dimer.";
P::usage="Variables for perfect matchings in the print mode.";
p::usage="Variables for perfect matchings in evaluation";
(*iterset2::usage="Iteration list of partial resolutions.";
iterset::usage="Iteration list of partial resolutions";*)

DimerGraph::usage="The brane tiling of the rectangular dimer.";
KMatrix::usage="The Kasteleyn matrix of a dimer model.";


(* Definitions of functions. *)
Begin["`Private`"]

DPrint[object_,debugFlag_]:=If[debugFlag==True,Print[object];]

RecDimerModels[n_Integer,m_Integer]:=Block[
{blacknodes,whitenodes,M1,M2,E1,E2,Edges,indexmap,allnodes,edgs,FindAdjNodes,Faces,NumEdgesperFace,faces,init,facemap,Fields,virtualpts,blackpos,whitepos,M11,M22,blackinplot,whiteinplot,blackmapping,whitemapping,leftbottom,lefttop,rightbottom,righttop,OutsidePtQ,InsidePtQ,alllines,intersectlines,weightx,weighty,allfaces,faceinfo,blacksur,whitesur,blacks,whites,backruleblack,backrulewhite,p1,p2,tem,Kmatrix,weightinfo,b,w,mygraphics,xrange,yrange,edges,pos,gralines,grablacknodes,grawhitenodes,grablacktexts,grawhitetexts,grafaces,gratoricunit,graintersectlines,upsquare,rightsquare,leftsquare,downsquare,up,right,left,down,facegroup,fieldgroup,x,y,anomap,face1,face2,face3,face4},
(* all black and white nodes. *)
blacknodes=Flatten[Table[b[i,j],{i,1,n},{j,1,m}]];
whitenodes=Flatten[Table[w[i,j],{i,1,n},{j,1,m}]];
(* count the indices *)
M1[r_]:=Mod[r,n]/.{0->n};
M2[c_]:=Mod[c,m]/.{0->m};
(* define the edges. *)
E1=Flatten[Table[{UndirectedEdge[b[i,j],w[M1[i-1],M2[j]]],UndirectedEdge[b[i,j],w[M1[i],M2[j-1]]]},{i,1,n},{j,1,m}]];
E2=Flatten[Table[{UndirectedEdge[w[i,j],b[M1[i+1],M2[j+1]]],UndirectedEdge[w[i,j],b[M1[i],M2[j]]]},{i,1,n},{j,1,m}]];
Edges=Union[E1,E2];
(* translate the notation for edgs. *)
indexmap=Union[(Rule@@#)&/@Transpose[{blacknodes,Array[B[#]&,Length[blacknodes]]}],(Rule@@#)&/@Transpose[{whitenodes,Array[W[#]&,Length[whitenodes]]}]];
Edges=Edges/.indexmap;
allnodes=Union[blacknodes,whitenodes]/.indexmap;
edges=((List@@#)&/@Edges);
(* looking for faces. *)
anomap=indexmap/.{b[x_,y_]:>{M1[x],M2[y]},w[x_,y_]:>{M1[x+0.5],M2[y+0.5]}};
Faces=Union[Sort/@Flatten[#,1]&@Table[
one=one/.b->List;face1={M1[#[[1]]],M2[#[[2]]]}&/@((#+one)&/@{{0,0},{0.5,0.5},{-0.5,0.5},{0,1}})/.anomap;
face2={M1[#[[1]]],M2[#[[2]]]}&/@((#+one)&/@{{0,0},{1,0},{0.5,0.5},{0.5,-0.5}})/.anomap;
face3={M1[#[[1]]],M2[#[[2]]]}&/@((#+one)&/@{{0,0},{-1,0},{-0.5,0.5},{-0.5,-0.5}})/.anomap;
face4={M1[#[[1]]],M2[#[[2]]]}&/@((#+one)&/@{{0,0},{0,-1},{-0.5,-0.5},{0.5,-0.5}})/.anomap;
{face1,face2,face3,face4}
,{one,blacknodes}]];
facemap=(Rule@@#)&/@Transpose[{Faces,Array[F[#]&,Length[Faces]]}];
Fields=(Table[DeleteCases[(If[SubsetQ[#,List@@one],#]&/@Faces)/.facemap,Null],{one,Edges}]/.F[x_]->x)/.{x_,y_}->Subscript[X, {x,y}];
(* start define the graphical objects. *)
xrange=-6;
yrange=Max[{2n+4,2m+4}];
virtualpts=Flatten[Table[{i,j},{i,xrange,yrange},{j,xrange,yrange}],1];
blackpos=2(blacknodes/.b->List);
whitepos=(#+{1,1})&/@blackpos;
M11[r_]:=2M1[r];
M22[c_]:=2M2[c];
blackinplot=DeleteCases[If[Cases[blackpos,{M11[#[[1]]/2],M22[#[[2]]/2]}]!={},#]&/@virtualpts,Null];
whiteinplot=(#+{1,1})&/@blackinplot;
blackmapping=(Rule@@#)&/@Transpose[{blackinplot,b[Flatten[Position[blackpos,{M11[#[[1]]/2],M22[#[[2]]/2]}]][[1]]]&/@blackinplot}];
whitemapping=(((#->(#-{1,1}))/.blackmapping)&/@whiteinplot)/.b->w;
(* boundaries of the Graphic[] *)
leftbottom={-0.5,-0.5};
lefttop=leftbottom+{0,2 m};
rightbottom=leftbottom+{2 n,0};
righttop=leftbottom+{2 n, 2m};
OutsidePtQ[x_,y_]:=(x<leftbottom[[1]]||x>rightbottom[[1]])||(y<leftbottom[[2]]||y>lefttop[[2]]);
InsidePtQ[x_,y_]:=(x>leftbottom[[1]]&&x<rightbottom[[1]])&&(y>leftbottom[[2]]&&y<lefttop[[2]]);
alllines=Flatten[Table[Tuples[{{one},((#+one)&/@{{-1,-1},{-1,1},{1,-1},{1,1}})}],{one,whiteinplot}],1];
intersectlines=DeleteCases[If[InsidePtQ@@#[[1]]!=InsidePtQ@@#[[2]],#]&/@alllines,Null];
intersectlines=DeleteCases[If[InsidePtQ@@#[[1]]==True,#]&/@intersectlines,Null];
weightx[intpts_]:=If[(intpts[[2]][[1]]>leftbottom[[1]])&&(intpts[[1]][[1]]<leftbottom[[1]]),x^-1,If[(intpts[[2]][[1]]>rightbottom[[1]])&&(intpts[[1]][[1]]<rightbottom[[1]]),x,0]];
weighty[intpts_]:=If[(intpts[[2]][[2]]>leftbottom[[2]])&&(intpts[[1]][[2]]<leftbottom[[2]]),y^-1,If[(intpts[[2]][[2]]>righttop[[2]])&&(intpts[[1]][[2]]<righttop[[2]]),y,0]];
(* looking for faces in the graph. *)
allfaces=Sort/@(Faces/.{B->b,W->w});
faceinfo=Flatten[DeleteCases[Table[face1=((#+one)&/@{{0,0},{1,1},{-1,1},{0,2}});
face2=((#+one)&/@{{0,0},{2,0},{1,1},{1,-1}});
face3=((#+one)&/@{{0,0},{-2,0},{-1,1},{-1,-1}});
face4=((#+one)&/@{{0,0},{0,-2},{-1,-1},{1,-1}});
DeleteCases[{If[(tem=Position[allfaces,Sort[face1/.blackmapping/.whitemapping]])!={},Text[Style[Flatten[tem][[1]],Small,Red],Total[face1]/4]],
If[(tem=Position[allfaces,Sort[face2/.blackmapping/.whitemapping]])!={},Text[Style[Flatten[tem][[1]],Small,Red],Total[face2]/4]],
If[(tem=Position[allfaces,Sort[face3/.blackmapping/.whitemapping]])!={},Text[Style[Flatten[tem][[1]],Small,Red],Total[face3]/4]],
If[(tem=Position[allfaces,Sort[face4/.blackmapping/.whitemapping]])!={},Text[Style[Flatten[tem][[1]],Small,Red],Total[face4]/4]]},Null]
,{one,blackinplot}],{}]];
Fields=Union[Flatten[Table[
upsquare=((#+one)&/@{{0,0},{0,2},{-1,1},{1,1}});
rightsquare=((#+one)&/@{{0,0},{2,0},{1,1},{1,-1}});
leftsquare=((#+one)&/@{{0,0},{-2,0},{-1,1},{-1,-1}});
downsquare=((#+one)&/@{{0,0},{0,-2},{-1,-1},{1,-1}});
{up,right,left,down}=DeleteCases[({upsquare,rightsquare,leftsquare,downsquare}/.blackmapping/.whitemapping),{_,_},Infinity];
If[Length[Flatten[{up,right,left,down}]]==16,{up,right,left,down}=Sort/@{up,right,left,down};
facegroup=Flatten[Position[allfaces,#]&/@{up,right,down,left}];
fieldgroup=Table[Subscript[X, {facegroup[[i]],facegroup[[If[i+1==5,1,i+1]]]}],{i,1,4}];
];
fieldgroup
,{one,blackinplot}]]];
Kmatrix=Table[0,{i,1,n m},{j,1 ,n m}];
Do[pos=Sort[Intersection@@(allfaces[[#]]&/@(one/.Subscript[X,List[x_,y_]]->{x,y}))]/.b[x_]->x/.w[y_]->y;
Kmatrix[[pos[[1]],pos[[2]]]]+=one;,{one,Fields}];
(* add on the weights. *)
weightinfo=Flatten[({{Sort[#/.blackmapping/.whitemapping],weightx[#],weighty[#]}&/@intersectlines/.{b[x_],w[y_]}->{x,y}}/.{0->1}),1];
Do[pos=one[[1]];Kmatrix[[pos[[1]],pos[[2]]]]*=one[[2]]one[[3]],{one,weightinfo}];
(* start organizing the graphic objects *)
gralines=Table[Graphics[Line/@Tuples[{{one},((#+one)&/@{{-1,-1},{-1,1},{1,-1},{1,1}})}]],{one,whiteinplot}];
grablacknodes=Graphics[{PointSize[0.02],Point[blackinplot,VertexColors->Black]}];
grawhitenodes=Graphics[Circle[#,0.1]&/@whiteinplot];
grablacktexts=Graphics[Text[Style[#/.blackmapping,Tiny,Blue],#+{0,0.3}]&/@blackinplot];
grawhitetexts=Graphics[Text[Style[#/.whitemapping,Tiny,Blue],#+{0,0.3}]&/@whiteinplot];
grafaces=Graphics[faceinfo];
gratoricunit=Graphics[{Dashed,Thick,LightRed,#}&/@{Line[{leftbottom,rightbottom}],Line[{leftbottom,lefttop}],Line[{rightbottom,righttop}],Line[{lefttop,righttop}]}];
graintersectlines=Graphics[{Dashed,Thick,Green,#}&/@{Line[#]&/@intersectlines}];
mygraphics=Show[gralines,grablacknodes,grawhitenodes,grablacktexts,grawhitetexts,grafaces,gratoricunit,graintersectlines,PlotRange->Transpose[{leftbottom-{2,2},righttop+{2,2}}]];
If[FileExistsQ["~/.dimer.model.tmp.txt"],DeleteFile["~/.dimer.model.tmp.txt"]];
Save["~/.dimer.model.tmp.txt",{blacknodes,whitenodes,M1,M2,E1,E2,Edges,indexmap,allnodes,edges,Faces,facemap,Fields,xrange,yrange,virtualpts,blackpos,whitepos,M11,M22,blackinplot,whiteinplot,blackmapping,whitemapping,leftbottom,lefttop,rightbottom,righttop,OutsidePtQ,InsidePtQ,alllines,intersectlines,allfaces,faceinfo,Kmatrix,weightinfo,mygraphics,gralines,grablacknodes,grawhitenodes,grablacktexts,grawhitetexts,grafaces,gratoricunit,graintersectlines}];
Return[{DimerGraph->mygraphics,KMatrix->Kmatrix}];
]

ToricInfo[Kmatrix_,InteractQ_:False]:=Block[{poly,coes,toricpts,toricdiagram,perfectmatchings,matchingrules,inversmatchingrules,toricpmcorresp,fieldexistinfo,fields,Pmatrix,TP,P2,P3,x,y,p},
poly=Det[Kmatrix];
coes=CoefficientRules[poly,{x,y}];
toricpts=#[[1]]&/@coes;
toricdiagram=Show[Graphics[Text[Style[#,Red],#+{0,0.2}]&/@toricpts],ListPlot[#[[1]]&/@coes,PlotMarkers->Automatic]];
perfectmatchings=Flatten[Union[#[[2]]&/@coes]/.Plus->List];
perfectmatchings=perfectmatchings/.Times[-1,x_]->Times[x];
fields=Variables[perfectmatchings];
matchingrules=Table[perfectmatchings[[ind]]->p[ind],{ind,1,Length[perfectmatchings]}];
inversmatchingrules=Reverse[#]&/@matchingrules;
toricpmcorresp=coes/.matchingrules;
fieldexistinfo=Table[f->DeleteCases[(If[Cases[#,f]!={},#]&/@perfectmatchings)/.matchingrules,Null],{f,fields}];
Pmatrix=Table[If[Position[p,f,Infinity]!={},1,0],{f,fields},{p,perfectmatchings}];
TP=Transpose[Pmatrix];
P2=Transpose[Table[Prepend[Pmatrix[[ind]],fields[[ind]]],{ind,1,Length[fields]}]];
P3=Transpose[Table[Prepend[P2[[ind+1]],P[ind]],{ind,0,Length[perfectmatchings]}]/.P[0]->"X/P"];
If[InteractQ==True,Print["K Matrix is: "];
Print[Kmatrix//MatrixForm];
Print["P Matrix is: "];
Print[P3//MatrixForm];
Print["Toric Diagram: "];
Print[toricdiagram];
Print["Each toric point contains the following perfect matchings: "];
Print[toricpmcorresp];
Print["Each field exists in the following perfect matchings: "];
Print[fieldexistinfo];];
Return[{KMatrix->Kmatrix,PMatrix->Pmatrix,PerfectMatchings->perfectmatchings,ToricDiagram->toricdiagram,ToricPerfMap->toricpmcorresp,FieldsPerfInfo->fieldexistinfo,ToricPts->toricpts}];
]

DrawPerfectMatching[oneperfmatching_]:=Block[{blacknodes,whitenodes,M1,M2,E1,E2,Edges,indexmap,allnodes,edges,Faces,facemap,Fields,xrange,yrange,virtualpts,blackpos,whitepos,M11,M22,blackinplot,whiteinplot,blackmapping,whitemapping,leftbottom,lefttop,rightbottom,righttop,OutsidePtQ,InsidePtQ,alllines,intersectlines,allfaces,faceinfo,Kmatrix,weightinfo,mygraphics,gralines,grablacknodes,grawhitenodes,grablacktexts,grawhitetexts,grafaces,gratoricunit,graintersectlines,matchindices,perfmatchmap,graperf,faceabove,facebelow,n1,n2},Get["~/.dimer.model.tmp.txt"];
matchindices=Sort/@(List@@oneperfmatching/.Subscript[X, {n1_,n2_}]->{n1,n2});
perfmatchmap=(Sort[#[[1]]]->#[[2]])&/@(facemap/.{B->b,W->w});
graperf=Graphics[{Thick,Dashed,Green,Line[#]}]&/@DeleteCases[Table[If[Divide@@((#[[2]]-#[[1]])&@one)==1,faceabove=Sort[DeleteCases[Flatten[{one,(#+{-1,1})&/@one},1]/.blackmapping/.whitemapping,{_,_}]];
facebelow=Sort[DeleteCases[Flatten[{one,(#+{1,-1})&/@one},1]/.blackmapping/.whitemapping,{_,_}]];
,
(* the other direction *)
faceabove=Sort[DeleteCases[Flatten[{one,(#+{1,1})&/@one},1]/.blackmapping/.whitemapping,{_,_}]];
facebelow=Sort[DeleteCases[Flatten[{one,(#+{-1,-1})&/@one},1]/.blackmapping/.whitemapping,{_,_}]];
];
;If[Length[faceabove]==4&&Length[facebelow]==4,(* the faces exist in the graph *)
If[Position[matchindices,Sort[{faceabove,facebelow}/.perfmatchmap/.F[x_]->x]]!={},one]]
,{one,alllines}],Null];
Return[Show[gralines,grablacknodes,grawhitenodes,grablacktexts,grawhitetexts,grafaces,gratoricunit,graperf,PlotRange->Transpose[{leftbottom-{2,2},righttop+{2,2}}]]];
]

TriangDimer[KM_,opts_]:=Module[{newopt,triandimer,area,tpts,newtoricdiag},
newopt=opts;
triandimer=DelaunayMesh[#[[1]]&/@CoefficientRules[Det[KM/.((#->0)&/@opts)],{x,y}],PlotTheme->"Monochrome"];
area=MeshCellCount[triandimer][[3]];
tpts=MeshCoordinates[triandimer];
newtoricdiag=Show[Graphics[Text[Style[StringReplace[ToString[IntegerPart/@#],{"{"->"(","}"->")"}],Black],#+{0,0.2}]&/@tpts],ListPlot[tpts,PlotMarkers->Automatic,PlotTheme->"Monochrome"]];
Return[{RemoveAntz->newopt,NewToricTiang->triandimer,NewToricDiag->newtoricdiag,DimerArea->area}];
]

RemovePointsParallel[Kmatrix_,ptsrm_]:=Block[{poly,coes,toricpts,toricdiagram,perfectmatchings,matchingrules,inversmatchingrules,toricpmcorresp,fieldexistinfo,fields,Pmatrix,TP,possiblechoices,rightchoices,removeinfo,ptstoremove,initlist,x,y,p,fieldlist,iterset,iterset2,iterlen,wronglist,GoodAntz,RightAntz,fieldrow,indexhead,goodchoice1(*,goodchoicenew*),goodchoice2,rightchoice,toricptcolrem,toricptcolrm,choices,data,PSow,temp={},len},
poly=Det[Kmatrix];
coes=CoefficientRules[poly,{x,y}];
toricpts=#[[1]]&/@coes;
toricdiagram=Show[Graphics[Text[Style[#,Red],#+{0,0.2}]&/@toricpts],ListPlot[#[[1]]&/@coes,PlotMarkers->Automatic]];
perfectmatchings=Flatten[Union[#[[2]]&/@coes]/.Plus->List];
perfectmatchings=perfectmatchings/.Times[-1,x_]->Times[x];
fields=Variables[perfectmatchings];
matchingrules=Table[perfectmatchings[[ind]]->p[ind],{ind,1,Length[perfectmatchings]}];
inversmatchingrules=Reverse[#]&/@matchingrules;
toricpmcorresp=coes/.matchingrules;
fieldexistinfo=Table[f->DeleteCases[(If[Cases[#,f]!={},#]&/@perfectmatchings)/.matchingrules,Null],{f,fields}];
Pmatrix=Table[If[Position[p,f,Infinity]!={},1,0],{f,fields},{p,perfectmatchings}];
If[Depth[ptsrm]==2,ptstoremove={ptsrm},ptstoremove=ptsrm];
fieldlist=Union[Flatten[(List@@#)&/@(Cases[{ptstoremove/.toricpmcorresp},p[_],Infinity]/.p[x_]:>(p[x]/.inversmatchingrules))]];
(*row numbers of fields to star in P matrix*)
fieldrow=Flatten[Position[fields,#]&/@fieldlist];
(*column number of the PM of remaining toric points*) 
toricptcolrem=((If[Head[#]===List,#,{#}]&/@(toricpmcorresp/.Times[-1,x_]->Times[x]/.Plus->List/.p[x_]->x/.({__}->x_)->x))[[Flatten[Position[toricpts,#]&/@Complement[toricpts,ptstoremove]]]]);
(*column number of the PM of points to be deleted*)
toricptcolrm=((If[Head[#]===List,#,{#}]&/@(toricpmcorresp/.Times[-1,x_]->Times[x]/.Plus->List/.p[x_]->x/.({__}->x_)->x))[[Flatten[Position[toricpts,#]&/@ptstoremove]]]);

(*check no additional points are deleted*)
(*indexhead:=If[Head[#]===List,#,{#}]&;*)
goodchoice1[choice_]:=(If[Union[#]=={True},True,False])&@(If[Union[#]=={False},False,True]&/@Map[If[#==0,True,False]&,Map[Total,(Transpose/@Transpose@(Table[Pmatrix[[ind,#]]&/@toricptcolrem,{ind,(*indexhead@*)choice}])),{2}],{2}]);
(*check if the points to be removed are indeed removed*)
goodchoice2[choice_]:=(If[Union[#]=={True},True,False])&@(If[Union[#]=={True},True,False]&/@Map[If[#>0,True,False]&,Map[Total,(Transpose/@Transpose@(Table[Pmatrix[[ind,#]]&/@toricptcolrm,{ind,(*indexhead@*)choice}])),{2}],{2}]);

iterset=DeleteCases[If[goodchoice1[{#}],#]&/@fieldrow,Null];
iterlen=Length[iterset];
initlist={};
wronglist={#}&/@Complement[fieldrow,iterset];
If[goodchoice2[{#}],AppendTo[initlist,#]]&/@fieldrow;
iterset2=iterset;
SetSharedFunction[PSow];
PSow[x_]:=Sow[x];
DistributeDefinitions[iterset,iterset2,wronglist]; (* distribute the definitions into different kernels. *)
Do[
iterset=DeleteCases[ParallelMap[If[Position[Flatten[Outer[SubsetQ,{#},wronglist,1]],True]=={},#]&,(DeleteCases[ParallelMap[If[Length[#]==ind,#]&,DeleteDuplicates[Union/@Flatten[ParallelMap[Flatten/@Tuples[{{#},iterset2}]&,iterset],1]]],Null])],Null];
wronglist=wronglist~Join~Flatten[(Reap[iterset=DeleteCases[ParallelMap[If[goodchoice1[#],#,PSow[#];]&,iterset],Null]][[2]]),1];
(*This finds all possible choices*)
(*initlist=initlist~Join~Flatten[(Reap[ParallelMap[If[goodchoice2[#],PSow[#];]&,iterset]][[2]]),1];*)
(*This finds some instances*)
temp=Flatten[(Reap[ParallelMap[If[goodchoice2[#],PSow[#];]&,iterset]][[2]]),1];
Print[ind," of ",iterlen," ,temp Length = ",Length[temp]," No. of fields in each choice = ", ind," ,Length of wronglist = ",Length[wronglist]];
(*If the correct choices of certain length are bigger than 2,then chop the size down to 1/6 of its original*)
If[Length[temp]>0,iterset=temp[[1;;Ceiling@(Length[temp]/10)]];initlist=initlist~Join~temp;];
temp={};
(*Print[initlist];*)
,{ind,2,iterlen}];(*][[2,1]];*)
(*This returns the last 10 elements from the result if its size is larger than 10*)
If[Length[initlist]>10,
fields[[#]]&/@(initlist[[Length[initlist]-9;;Length[initlist]]]),fields[[#]]&/@initlist]
]//AbsoluteTiming;

HiggsingDimerSU[opts_]:=Block[{blacknodes,whitenodes,M1,M2,E1,E2,Edges,indexmap,allnodes,edges,Faces,facemap,Fields,xrange,yrange,virtualpts,blackpos,whitepos,M11,M22,blackinplot,whiteinplot,blackmapping,whitemapping,leftbottom,lefttop,rightbottom,righttop,OutsidePtQ,InsidePtQ,alllines,intersectlines,allfaces,faceinfo,Kmatrix,weightinfo,mygraphics,gralines,grablacknodes,grawhitenodes,grablacktexts,grawhitetexts,grafaces,gratoricunit,graintersectlines,matchindices,perfmatchmap,graperf,faceabove,facebelow,reversefacemap,newlines,granewlines,nornewlines,nodesinplot,newblackinplot,newwhiteinplot,newgrablacknodes,newgrawhitenodes,newgrablacktexts,newgrawhitetexts,newdimergraph,x,X,rempts,allinds,NFmap,mattercontentinfo,superterms1,superterms2,superpotential,linetofieldmap,PositiveVecQ,phyblackunit,phywhiteunit,temp1,temp2,quiverarrows,quivergraph,OrderField,whitelist,blacklist,whiteitems,blackitems,AllFields,forthinds,backinds,omegab,omegaa,h,eles,testeles,testlist,conq,continfo,omegaainv,PosInds,NegInds,Z,DrawZZ,stack,zigzagpaths,GetBlackCycles,GetWhiteCycles,nfaces,simpfacemap,cont,quivergraph1,nodes,newcontinfo,linkinfo,newnodesinplot,massivenodes,reducenode,joinednode,newnode,indstoremove,geopos,posinnewlines,postoredirect,newnodegeo,newnewdimer,newgrawhitenodes2,anotherdimer,dimer1,dimer2},
Get["~/.dimer.model.tmp.txt"];
rempts=opts/.

\!\(\*SubscriptBox[\(X\), \({a_, b_}\)]\):>Sort[{a,b}];
If[Head[rempts]!=List,rempts={rempts}];
reversefacemap=(Sort[#[[1]]]->#[[2]])&/@(facemap/.{B->b,W->w});
mattercontentinfo={};
newlines=DeleteCases[Table[If[Divide@@((#[[2]]-#[[1]])&@one)==1,faceabove=Sort[DeleteCases[Flatten[{one,(#+{-1,1})&/@one},1]/.blackmapping/.whitemapping,{_,_}]];
facebelow=Sort[DeleteCases[Flatten[{one,(#+{1,-1})&/@one},1]/.blackmapping/.whitemapping,{_,_}]];
,
(* the other direction *)
faceabove=Sort[DeleteCases[Flatten[{one,(#+{1,1})&/@one},1]/.blackmapping/.whitemapping,{_,_}]];
facebelow=Sort[DeleteCases[Flatten[{one,(#+{-1,-1})&/@one},1]/.blackmapping/.whitemapping,{_,_}]];
];
 If[Length[faceabove]==4&&Length[facebelow]==4,
(* the faces exist in the graph *)
If[Position[rempts,Sort[{faceabove,facebelow}/.reversefacemap/.F[x_]->x]]=={},AppendTo[mattercontentinfo,{Sort[one],Sort[{faceabove,facebelow}/.reversefacemap/.F[x_]->x]}];one]]
,{one,alllines}],Null];
granewlines=Graphics[{Thick,Dashed,Blue,Line[#]}]&/@newlines;
nornewlines=Sort/@newlines;
nodesinplot=DeleteCases[Table[If[Position[nornewlines,#]&/@Sort/@Tuples[{{one},((#+one)&/@{{-1,-1},{-1,1},{1,-1},{1,1}})}]=={{},{},{},{}},(* not in the graph *)Null,(* in the graph *)one],{one,virtualpts}],Null];
newblackinplot=Intersection[blackinplot,nodesinplot];
newwhiteinplot=Intersection[whiteinplot,nodesinplot];
newgrablacknodes=Graphics[{PointSize[0.02],Point[newblackinplot,VertexColors->Black]}];
newgrawhitenodes=Graphics[Circle[#,0.1]&/@newwhiteinplot];
newgrablacktexts=Graphics[Text[Style[#/.blackmapping,Tiny,Blue],#+{0,0.3}]&/@newblackinplot];
newgrawhitetexts=Graphics[Text[Style[#/.whitemapping,Tiny,Blue],#+{0,0.3}]&/@newwhiteinplot];
newdimergraph=Show[granewlines,newgrablacknodes,newgrawhitenodes,newgrablacktexts,newgrawhitetexts,grafaces,gratoricunit];
(* super W *)
allinds={#}&/@Sort[Union[Flatten[rempts]]];
linetofieldmap=((#/.(

\!\(\*SubscriptBox[\(X\), \({a_, b_}\)]\):>Sort[{a,b}]))->#)&/@Fields;
allinds=Sort/@Union[Table[temp1=one;temp2={};
While[temp1!=temp2,
temp2=temp1;
If[Intersection[temp1,#]!={},temp1=Union[temp1,#];]&/@rempts;];temp1,{one,allinds}]];
NFmap=(#->StringDelete[ToString[#],{",","{","}"," "}])&/@Union[Sort/@allinds];
NFmap=(Rule@@#)&/@Flatten[Tuples[{#[[1]],{#[[2]]}}]&/@NFmap,1];
(* transform all the number indices into string indices. *)
NFmap=Union[(#->ToString[#])&/@(Complement[Range[Length[facemap]],#[[1]]&/@NFmap]),NFmap];
PositiveVecQ[vec_]:=And@@((#>0)&/@vec);
phyblackunit=newblackinplot[[Flatten[Position[(PositiveVecQ[#-leftbottom]&&PositiveVecQ[righttop-#])&/@newblackinplot,True]]]];
phywhiteunit=newwhiteinplot[[Flatten[Position[(PositiveVecQ[#-leftbottom]&&PositiveVecQ[righttop-#])&/@newwhiteinplot,True]]]];
superterms1=Union[DeleteCases[Table[(If[#!={},-1*Times@@#])&@(Sort/@DeleteCases[If[Position[#[[1]],one]!={},#[[2]]]&/@mattercontentinfo,Null]/.linetofieldmap),{one,phywhiteunit}],Null]/.NFmap];
superterms2=Union[DeleteCases[Table[(If[#!={},Times@@#])&@(Sort/@DeleteCases[If[Position[#[[1]],one]!={},#[[2]]]&/@mattercontentinfo,Null]/.linetofieldmap),{one,phyblackunit}],Null]/.NFmap];
superpotential=Total[superterms1]+Total[superterms2];
(* now check the consistency of the model *)
continfo=Transpose[{(#[[1]]&/@mattercontentinfo)/.blackmapping/.whitemapping,(#[[2]]&/@mattercontentinfo)/.linetofieldmap/.NFmap}];
continfo=DeleteDuplicates[continfo];
continfo=Transpose[{Sort[#[[1]]]&/@continfo,#[[2]]&/@continfo}];
GetBlackCycles[blackpts_]:=Module[{temp,linkedanticlock,linkedpts},{linkedanticlock=(#+blackpts)&/@{{1,1},{-1,1},{-1,-1},{1,-1}};
linkedpts=mattercontentinfo[[Flatten[#[[1]]&/@Position[nornewlines,blackpts]]]];
temp=#[[1]]&/@linkedpts;
linkedpts=linkedpts[[Flatten[((#[[1]]&/@Position[temp,#])&/@linkedanticlock)]]];
{Sort[(#[[1]]/.blackmapping/.whitemapping)],#[[2]]/.linetofieldmap/.NFmap}&/@linkedpts
}];
GetWhiteCycles[whitepts_]:=Module[{temp,linkedanticlock,linkedpts},{linkedanticlock=(#+whitepts)&/@{{1,1},{1,-1},{-1,-1},{-1,1}};
linkedpts=mattercontentinfo[[Flatten[#[[1]]&/@Position[nornewlines,whitepts]]]];
temp=#[[1]]&/@linkedpts;
linkedpts=linkedpts[[Flatten[((#[[1]]&/@Position[temp,#])&/@linkedanticlock)]]];
{Sort[(#[[1]]/.blackmapping/.whitemapping)],#[[2]]/.linetofieldmap/.NFmap}&/@linkedpts
}];
blackitems=Flatten[GetBlackCycles[#]&/@phyblackunit,1];
whiteitems=Flatten[GetWhiteCycles[#]&/@phywhiteunit,1];
AllFields=continfo;
forthinds=(Rule@@#)&/@Transpose[{AllFields,Range[Length[AllFields]]}];
backinds=Reverse/@forthinds;
(*omegab=InversePermutation[Cycles[blackitems/.forthinds]];
omegaa=Cycles[whiteitems/.forthinds];
omegaainv=InversePermutation[omegaa];
PosInds={"+",#}&/@Range[Length[AllFields]];
NegInds={"-",#}&/@Range[Length[AllFields]];
Z[{"-",k_}]:={"+",PermutationReplace[k,omegab]};
Z[{"+",k_}]:={"-",PermutationReplace[k,omegaainv]};
DrawZZ[list_]:=If[Position[list,temp=Z[Last[list]]]\[NotEqual]{},list,Append[list,temp]];
stack=Union[PosInds,NegInds];
zigzagpaths={};
While[stack\[NotEqual]{},AppendTo[zigzagpaths,temp=FixedPoint[DrawZZ,{stack[[1]]}]];stack=Complement[stack,temp];];
conq=And@@(DuplicateFreeQ[#[[2]]&/@#]&/@zigzagpaths);*)
(*h=PermutationProduct[omegab,InversePermutation[omegaa]];
eles=GroupElements[PermutationGroup[{h}]];
testeles=PermutationProduct[#,omegab]&/@eles;
testlist=#[[1]]&/@testeles;
conq=If[Flatten[Complement[Range[Length[AllFields]],#]&/@(Flatten/@testlist)]\[Equal]{},True,False];*)
(* generate the output *)
(*quiverarrows=(#[[2]]&/@continfo)/.Subscript[X, {a_,b_}]\[RuleDelayed]DirectedEdge[a,b];
quivergraph1=Graph[quiverarrows,VertexLabels\[Rule]"Name"];
nfaces=Union[Flatten[(#[[2]]&/@continfo)/.Subscript[X,List[x_,y_]]:>{x,y}]];
simpfacemap=(Rule@@#)&/@Transpose[{nfaces,(ToString[#])&/@Range[Length[nfaces]]}];
superpotential=superpotential/.simpfacemap;
cont=continfo/.simpfacemap;
quiverarrows=(#[[2]]&/@cont)/.Subscript[X, {a_,b_}]\[RuleDelayed]DirectedEdge[a,b];
quivergraph=Graph[quiverarrows,VertexLabels\[Rule]"Name"];*)
(*****)
nodes=allnodes/.{B->b,W->w};
newcontinfo=continfo;
nodes=nodes/.b[x_]:>ToString[b[x]]/.w[x_]:>ToString[w[x]];
newcontinfo=newcontinfo/.b[x_]:>ToString[b[x]]/.w[x_]:>ToString[w[x]];
linkinfo=#[[1]]&/@newcontinfo;
newnodesinplot={#,ToString[#/.blackmapping/.whitemapping]}&/@nodesinplot;
massivenodes=DeleteCases[If[Length[Position[linkinfo,#]]==2,#]&/@nodes,Null];
While[massivenodes!={},
reducenode=massivenodes[[1]];
joinednode=DeleteCases[Flatten[linkinfo[[deledges=Flatten[#[[1]]&/@Position[linkinfo,reducenode]]]]],reducenode];
newnode=StringJoin@@(ToString/@joinednode);
newcontinfo=Delete[newcontinfo,{#}&/@deledges]/.((#->newnode)&/@joinednode);
nodes=DeleteDuplicates[DeleteCases[nodes,reducenode]/.((#->newnode)&/@joinednode)];
linkinfo=#[[1]]&/@newcontinfo;
(* node & lines operation *)
indstoremove=Flatten[#[[1]]&/@Position[newnodesinplot,reducenode]];
While[indstoremove!={},
one=indstoremove[[1]];
geopos=newnodesinplot[[one]][[1]];
posinnewlines=newlines[[Flatten[#[[1]]&/@Position[newlines,geopos]]]];
postoredirect=Flatten[DeleteCases[#,geopos]]&/@posinnewlines;
If[Length[postoredirect]==2,
newnodegeo={Total[postoredirect]/2,newnode};
(* delete the old node  *)
newnodesinplot=Delete[newnodesinplot,{one}];
newnodesinplot=Delete[newnodesinplot,{#[[1]]}&/@(Flatten[Position[newnodesinplot,#]]&/@postoredirect)];
(* name the new node *)
newnodesinplot=AppendTo[newnodesinplot,newnodegeo];
(* handle the lines. \[Rule] remove and replace *)
newlines=ReplaceRepeated[Delete[newlines,{#[[1]]}&/@Position[newlines,geopos]],(#->newnodegeo[[1]])&/@postoredirect];
,
newnodegeo={Total[postoredirect],newnode};
newnodesinplot=Delete[newnodesinplot,{one}];
newnodesinplot=Delete[newnodesinplot,{#[[1]]}&/@(Flatten[Position[newnodesinplot,#]]&/@postoredirect)];
(* name the new node *)
newnodesinplot=AppendTo[newnodesinplot,newnodegeo];
];
indstoremove=Flatten[#[[1]]&/@Position[newnodesinplot,reducenode]];
];
(* compute new massive nodes *)
massivenodes=DeleteCases[If[Length[Position[linkinfo,#]]==2,#]&/@nodes,Null];
];
nfaces=Union[Flatten[(#[[2]]&/@newcontinfo)/.Subscript[X,List[x_,y_]]:>{x,y}]];
simpfacemap=(Rule@@#)&/@Transpose[{nfaces,(ToString[#])&/@Range[Length[nfaces]]}];
(*superpotential=superpotential/.simpfacemap;*)
cont=newcontinfo/.simpfacemap;
quiverarrows=(#[[2]]&/@cont)/.

\!\(\*SubscriptBox[\(X\), \({a_, b_}\)]\):>DirectedEdge[a,b];
quivergraph=Graph[quiverarrows,VertexLabels->"Name"];
(*superpotential=Total[If[StringCases[#,"b"]\[NotEqual]{},-1,1]*Times@@(#[[2]]&/@cont[[#[[1]]&/@Position[newcontinfo,#]]])&/@nodes];*)
superpotential=If[StringCases[#,"b"]!={},-1,1]*Times@@(#[[2]]&/@cont[[#[[1]]&/@Position[newcontinfo,#]]])&/@nodes;
(******)
granewlines=Graphics[{Thick,Black,Line[#]}]&/@newlines;
newblackinplot=DeleteCases[If[StringCases[#[[2]],"b"]!={},#]&/@newnodesinplot,Null];
newwhiteinplot=DeleteCases[If[StringCases[#[[2]],"w"]!={},#]&/@newnodesinplot,Null];
newgrablacknodes=Graphics[{PointSize[0.05],Point[#[[1]]&/@newblackinplot,VertexColors->Black]}];
newgrawhitenodes=Graphics[Circle[#,0.17]&/@(#[[1]]&/@newwhiteinplot)];
newgrawhitenodes2=Graphics[{White,Disk[#,{0.17,0.17}]}&/@(#[[1]]&/@newwhiteinplot)];
newgrablacktexts=Graphics[Text[Style[#[[2]]/.blackmapping,Blue],#[[1]]+{0,0.3}]&/@newblackinplot];
newgrawhitetexts=Graphics[Text[Style[#[[2]]/.whitemapping,Blue],#[[1]]+{0,0.3}]&/@newwhiteinplot];
grafaces=grafaces/.Style[x_,y__,z__]:>Style[x/.NFmap/.simpfacemap,y,z];
quivergraph=Graph[Graph@@({VertexList[#],EdgeList[#]}&@quivergraph),VertexSize->Medium,VertexLabels->Table[i->Placed[Style[i,Large],Center],{i,VertexList[quivergraph]}],VertexStyle->RGBColor[51/256,153/256,255/256],EdgeStyle->Black];
newnewdimer=Show[granewlines,newgrablacknodes,newgrawhitenodes,newgrawhitenodes2,grafaces,newgrablacktexts,newgrawhitetexts,gratoricunit/.RGBColor[1,0.85`,0.85`]->Red,PlotRange->Transpose[{leftbottom-{1,1},righttop+{2,2}}]];
(*dimer1=Show[granewlines,grafaces,PlotRange\[Rule]Transpose[{leftbottom-{1,1},righttop+{2,2}}]];
dimer2=Show[newgrablacknodes,newgrawhitenodes,PlotRange\[Rule]Transpose[{leftbottom-{1,1},righttop+{2,2}}]];
anotherdimer=ImageCompose[dimer1,{dimer2,1}];*)
Return[{HiggsingDimer->newnewdimer,SUW->superpotential,Quiver->quivergraph,Content->cont}]
(*start checking the consistency*)
]

DimerDemo[]:=Module[{},
Print["This is a simple demonstration of computing dP3 geometry."];
Print["Input: "];
Print["RecDimerModels[2,2]"];
Print["Output: "];
Print[RecDimerModels[2,2]];
Print["Now that we know the K matrix, we can use ToricInfo[] to compute the perfect matchings."];
Print["Input: "];
Print["ToricInfo[{{\!\(\*SubscriptBox[\(X\), \({1, 3}\)]\),\!\(\*SubscriptBox[\(X\), \({3, 2}\)]\),\!\(\*SubscriptBox[\(X\), \({4, 1}\)]\),\!\(\*SubscriptBox[\(X\), \({2, 4}\)]\)},{y \!\(\*SubscriptBox[\(X\), \({5, 1}\)]\),\!\(\*SubscriptBox[\(X\), \({2, 5}\)]\),y \!\(\*SubscriptBox[\(X\), \({1, 6}\)]\),\!\(\*SubscriptBox[\(X\), \({6, 2}\)]\)},{x \!\(\*SubscriptBox[\(X\), \({3, 7}\)]\),x \!\(\*SubscriptBox[\(X\), \({8, 3}\)]\),\!\(\*SubscriptBox[\(X\), \({7, 4}\)]\),\!\(\*SubscriptBox[\(X\), \({4, 8}\)]\)},{x y \!\(\*SubscriptBox[\(X\), \({7, 5}\)]\),x \!\(\*SubscriptBox[\(X\), \({5, 8}\)]\),y \!\(\*SubscriptBox[\(X\), \({6, 7}\)]\),\!\(\*SubscriptBox[\(X\), \({8, 6}\)]\)}}]"];
Print["Output: "];
Print[ToricInfo[{{
\!\(\*SubscriptBox[\(X\), \({1, 3}\)]\),
\!\(\*SubscriptBox[\(X\), \({3, 2}\)]\),
\!\(\*SubscriptBox[\(X\), \({4, 1}\)]\),
\!\(\*SubscriptBox[\(X\), \({2, 4}\)]\)},{y 
\!\(\*SubscriptBox[\(X\), \({5, 1}\)]\),
\!\(\*SubscriptBox[\(X\), \({2, 5}\)]\),y 
\!\(\*SubscriptBox[\(X\), \({1, 6}\)]\),
\!\(\*SubscriptBox[\(X\), \({6, 2}\)]\)},{x 
\!\(\*SubscriptBox[\(X\), \({3, 7}\)]\),x 
\!\(\*SubscriptBox[\(X\), \({8, 3}\)]\),
\!\(\*SubscriptBox[\(X\), \({7, 4}\)]\),
\!\(\*SubscriptBox[\(X\), \({4, 8}\)]\)},{x y 
\!\(\*SubscriptBox[\(X\), \({7, 5}\)]\),x 
\!\(\*SubscriptBox[\(X\), \({5, 8}\)]\),y 
\!\(\*SubscriptBox[\(X\), \({6, 7}\)]\),
\!\(\*SubscriptBox[\(X\), \({8, 6}\)]\)}}]];
Print["One can view a certain perfect matching using DrawPerfectMatching[]."];
Print["Input: "];
Print["DrawPerfectMatching[\!\(\*SubscriptBox[\(X\), \({4, 1}\)]\) \!\(\*SubscriptBox[\(X\), \({5, 1}\)]\) \!\(\*SubscriptBox[\(X\), \({8, 3}\)]\) \!\(\*SubscriptBox[\(X\), \({8, 6}\)]\)]"];
Print["Output: "];
Print[DrawPerfectMatching[
\!\(\*SubscriptBox[\(X\), \({4, 1}\)]\) 
\!\(\*SubscriptBox[\(X\), \({5, 1}\)]\) 
\!\(\*SubscriptBox[\(X\), \({8, 3}\)]\) 
\!\(\*SubscriptBox[\(X\), \({8, 6}\)]\)]];
Print["Next, we want to compute the higgsing of dP3 toric diagram, and we need to remove the points {0,2} and {2,0} from the original toric diagram."];
Print["Input: "];
Print["RemovePointsParallel[{{\!\(\*SubscriptBox[\(X\), \({1, 3}\)]\),\!\(\*SubscriptBox[\(X\), \({3, 2}\)]\),\!\(\*SubscriptBox[\(X\), \({4, 1}\)]\),\!\(\*SubscriptBox[\(X\), \({2, 4}\)]\)},{y \!\(\*SubscriptBox[\(X\), \({5, 1}\)]\),\!\(\*SubscriptBox[\(X\), \({2, 5}\)]\),y \!\(\*SubscriptBox[\(X\), \({1, 6}\)]\),\!\(\*SubscriptBox[\(X\), \({6, 2}\)]\)},{x \!\(\*SubscriptBox[\(X\), \({3, 7}\)]\),x \!\(\*SubscriptBox[\(X\), \({8, 3}\)]\),\!\(\*SubscriptBox[\(X\), \({7, 4}\)]\),\!\(\*SubscriptBox[\(X\), \({4, 8}\)]\)},{x y \!\(\*SubscriptBox[\(X\), \({7, 5}\)]\),x \!\(\*SubscriptBox[\(X\), \({5, 8}\)]\),y \!\(\*SubscriptBox[\(X\), \({6, 7}\)]\),\!\(\*SubscriptBox[\(X\), \({8, 6}\)]\)}},{{0,2},{2,0}}]"];
Print["Output: "];
Print[RemovePointsParallel[{{
\!\(\*SubscriptBox[\(X\), \({1, 3}\)]\),
\!\(\*SubscriptBox[\(X\), \({3, 2}\)]\),
\!\(\*SubscriptBox[\(X\), \({4, 1}\)]\),
\!\(\*SubscriptBox[\(X\), \({2, 4}\)]\)},{y 
\!\(\*SubscriptBox[\(X\), \({5, 1}\)]\),
\!\(\*SubscriptBox[\(X\), \({2, 5}\)]\),y 
\!\(\*SubscriptBox[\(X\), \({1, 6}\)]\),
\!\(\*SubscriptBox[\(X\), \({6, 2}\)]\)},{x 
\!\(\*SubscriptBox[\(X\), \({3, 7}\)]\),x 
\!\(\*SubscriptBox[\(X\), \({8, 3}\)]\),
\!\(\*SubscriptBox[\(X\), \({7, 4}\)]\),
\!\(\*SubscriptBox[\(X\), \({4, 8}\)]\)},{x y 
\!\(\*SubscriptBox[\(X\), \({7, 5}\)]\),x 
\!\(\*SubscriptBox[\(X\), \({5, 8}\)]\),y 
\!\(\*SubscriptBox[\(X\), \({6, 7}\)]\),
\!\(\*SubscriptBox[\(X\), \({8, 6}\)]\)}},{{0,2},{2,0}}]];
Print["Choosing one of the higgsing plan, we can compute the quiver graph, super-potential terms and field contents using HiggsingDimerSU[]."];
Print["Input: "];
Print["HiggsingDimerSU[{\!\(\*SubscriptBox[\(X\), \({4, 8}\)]\),\!\(\*SubscriptBox[\(X\), \({6, 2}\)]\)}]"];
Print["Output: "];
Print[HiggsingDimerSU[{
\!\(\*SubscriptBox[\(X\), \({4, 8}\)]\),
\!\(\*SubscriptBox[\(X\), \({6, 2}\)]\)}]];
Print["And view the triangulation of the dP3 toric diagram."];
Print["Input: "];
Print["TriangDimer[{{\!\(\*SubscriptBox[\(X\), \({1, 3}\)]\),\!\(\*SubscriptBox[\(X\), \({3, 2}\)]\),\!\(\*SubscriptBox[\(X\), \({4, 1}\)]\),\!\(\*SubscriptBox[\(X\), \({2, 4}\)]\)},{y \!\(\*SubscriptBox[\(X\), \({5, 1}\)]\),\!\(\*SubscriptBox[\(X\), \({2, 5}\)]\),y \!\(\*SubscriptBox[\(X\), \({1, 6}\)]\),\!\(\*SubscriptBox[\(X\), \({6, 2}\)]\)},{x \!\(\*SubscriptBox[\(X\), \({3, 7}\)]\),x \!\(\*SubscriptBox[\(X\), \({8, 3}\)]\),\!\(\*SubscriptBox[\(X\), \({7, 4}\)]\),\!\(\*SubscriptBox[\(X\), \({4, 8}\)]\)},{x y \!\(\*SubscriptBox[\(X\), \({7, 5}\)]\),x \!\(\*SubscriptBox[\(X\), \({5, 8}\)]\),y \!\(\*SubscriptBox[\(X\), \({6, 7}\)]\),\!\(\*SubscriptBox[\(X\), \({8, 6}\)]\)}},{\!\(\*SubscriptBox[\(X\), \({4, 8}\)]\),\!\(\*SubscriptBox[\(X\), \({6, 2}\)]\)}]"];
Print["Output: "];
Print[TriangDimer[{{
\!\(\*SubscriptBox[\(X\), \({1, 3}\)]\),
\!\(\*SubscriptBox[\(X\), \({3, 2}\)]\),
\!\(\*SubscriptBox[\(X\), \({4, 1}\)]\),
\!\(\*SubscriptBox[\(X\), \({2, 4}\)]\)},{y 
\!\(\*SubscriptBox[\(X\), \({5, 1}\)]\),
\!\(\*SubscriptBox[\(X\), \({2, 5}\)]\),y 
\!\(\*SubscriptBox[\(X\), \({1, 6}\)]\),
\!\(\*SubscriptBox[\(X\), \({6, 2}\)]\)},{x 
\!\(\*SubscriptBox[\(X\), \({3, 7}\)]\),x 
\!\(\*SubscriptBox[\(X\), \({8, 3}\)]\),
\!\(\*SubscriptBox[\(X\), \({7, 4}\)]\),
\!\(\*SubscriptBox[\(X\), \({4, 8}\)]\)},{x y 
\!\(\*SubscriptBox[\(X\), \({7, 5}\)]\),x 
\!\(\*SubscriptBox[\(X\), \({5, 8}\)]\),y 
\!\(\*SubscriptBox[\(X\), \({6, 7}\)]\),
\!\(\*SubscriptBox[\(X\), \({8, 6}\)]\)}},{
\!\(\*SubscriptBox[\(X\), \({4, 8}\)]\),
\!\(\*SubscriptBox[\(X\), \({6, 2}\)]\)}]];
]



End[]
EndPackage[];


