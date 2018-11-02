(* ::Package:: *)

(* This is an experimental package to implement vim mode in Mathematica Frontend *)


BeginPackage["ViMma`"];
ClearAll["ViMma`*"];
ClearAll["ViMma`Private`*"];


VimEdit::usage = "VimEdit[file_String] opens an existing file.";


Begin["`Private`"];


VimEdit[file_String] := Module[
	{
		state, notebook
	},
	
	notebook = NotebookOpen[file];
	state = <|"Mode" -> "Normal", "Notebook" -> notebook|>;
	SelectionMove[notebook, Before, CellContents];	
	SetOptions[notebook, NotebookEventActions-> GetEventHandler[state]];
	notebook
];


(* ::Section:: *)
(*EventHandler*)


GetEventHandler[state_Association] := Module[
	{
		newstate = state
	},
	
(*	{
		"KeyDown" :> Echo["Key " <> ToString[CurrentValue["EventKey"]] <> " Pressed. State: " <> ToString[curstate]]
	} ~Join~*)
	Switch[state["Mode"],
		"Normal", {
			"KeyDown" :> Nothing,
			{"KeyDown", "j"} :> CursorMove["Normal", "Down"],
			{"KeyDown", "k"} :> CursorMove["Normal", "Up"],
			{"KeyDown", "h"} :> CursorMove["Normal", "Left"],
			{"KeyDown", "l"} :> CursorMove["Normal", "Right"],
			{"KeyDown", "w"} :> CursorMove["Normal", "NextWord"],
			{"KeyDown", "b"} :> CursorMove["Normal", "PreviousWord"],
			{"KeyDown", "e"} :> CursorMove["Normal", "EndWord"],
			{"KeyDown", "x"} :> EditText["Normal", "Delete"],
			{"KeyDown", "i"} :> (
				newstate = SwitchMode["Normal", "Insert", state];
				FrontEndExecute[FrontEndToken["MovePrevious"]];
				Return[SetOptions[state["Notebook"], NotebookEventActions-> GetEventHandler[newstate]]];
			),
			PassEventsDown :> False
		},
		"Insert", {
			"EscapeKeyDown" :> ((*If[TrueQ[ReadyToNormalQ[state]],
				newstate = SwitchMode["Insert", "Normal", state];
				Return[SetOptions[state["Notebook"], NotebookEventActions-> GetEventHandler[newstate]]],
				Nothing
			]*)
				If[CurrentValue["ShiftKey"],
					NotebookWrite[state["Notebook"], FromCharacterCode[{63332}]],
					newstate = SwitchMode["Insert", "Normal", state];
					FrontEndExecute[FrontEndToken["MoveNext"]];
					FrontEndExecute[FrontEndToken["SelectPrevious"]];
					Return[SetOptions[state["Notebook"], NotebookEventActions-> GetEventHandler[newstate]]]
				];
			),
			PassEventsDown :> False
		},
		_, {}
	]
];


(* ::Section:: *)
(*Mode Switch*)


SwitchMode::wrmode = "Try to switch from `1` to `2`, but current state is `3`.";
VimModeQ = MatchQ[("Normal"|"Visual"|"Insert"|"Replace"|"Ex")];
SwitchMode[from_?VimModeQ, to_?VimModeQ, state_Association] := (
	If[state["Mode"] === from,
		ReplacePart[state, "Mode" -> to],
		Message[SwitchMode::wrmode, from, to ,state["Mode"]];
		state
	]
);

ReadyToNormalQ[state_Association] := Module[
	{
	
	},
	
	FrontEndExecute[FrontEndToken["Previous"]];
	FrontEndExecute[FrontEndToken["SelectPrevious"]];
	Abort[];
	If[ToCharacterCode[CurrentValue[state["Notebook"], "SelectionData"]] === {63332},
		FrontEndExecute[FrontEndToken["Clear"]];
(*		FrontEndExecute[FrontEndToken["SelectNext"]];
		FrontEndExecute[FrontEndToken["Clear"]];*)
		FrontEndExecute[FrontEndToken["Next"]];
		FrontEndExecute[FrontEndToken["SelectPrevious"]];
		True
		,
		FrontEndExecute[FrontEndToken["SelectNext"]];
		FrontEndExecute[FrontEndToken["Next"]];
		False
	]
];


(* ::Section:: *)
(*Normal Mode*)


(* ::Subsection:: *)
(*Cursor Movement*)


CursorMove["Normal", "Up"] := (
	FrontEndExecute[FrontEndToken[notebook,"MovePreviousLine"]];
	FrontEndExecute[FrontEndToken[notebook,"MoveNext"]];
	FrontEndExecute[FrontEndToken[notebook,"SelectPrevious"]];
);

CursorMove["Normal", "Down"] := (
	FrontEndExecute[FrontEndToken[notebook,"MoveNextLine"]];
	FrontEndExecute[FrontEndToken[notebook,"MoveNext"]];
	FrontEndExecute[FrontEndToken[notebook,"SelectPrevious"]];
);

CursorMove["Normal", "Left"] := (
	FrontEndExecute[FrontEndToken[notebook,"MovePrevious"]];
	FrontEndExecute[FrontEndToken[notebook,"SelectPrevious"]];
);

CursorMove["Normal", "Right"] := (
	FrontEndExecute[FrontEndToken[notebook,"SelectNext"]];
	FrontEndExecute[FrontEndToken[notebook,"MoveNext"]];
	FrontEndExecute[FrontEndToken[notebook,"SelectPrevious"]];
);
	
CursorMove["Normal", "NextWord"] := (
	FrontEndExecute[FrontEndToken[notebook,"MovePrevious"]];
	FrontEndExecute[FrontEndToken[notebook,"MoveNextWord"]];
	FrontEndExecute[FrontEndToken[notebook,"MoveNext"]];
	FrontEndExecute[FrontEndToken[notebook,"SelectPrevious"]];
);
	
CursorMove["Normal", "PreviousWord"] := (
	FrontEndExecute[FrontEndToken[notebook,"MovePrevious"]];
	FrontEndExecute[FrontEndToken[notebook,"MovePreviousWord"]];
	FrontEndExecute[FrontEndToken[notebook,"MoveNext"]];
	FrontEndExecute[FrontEndToken[notebook,"SelectPrevious"]];
);
	
CursorMove["Normal", "EndWord"] := (
	FrontEndExecute[FrontEndToken[notebook,"SelectNext"]];
	FrontEndExecute[FrontEndToken[notebook,"MoveNextWord"]];
	FrontEndExecute[FrontEndToken[notebook,"SelectPrevious"]];
);
	


(* ::Subsection:: *)
(*EditText*)


EditText["Normal", "Delete"] := (
	FrontEndExecute[FrontEndToken[notebook,"Cut"]];
	FrontEndExecute[FrontEndToken[notebook,"MoveNext"]];
	FrontEndExecute[FrontEndToken[notebook,"SelectPrevious"]];
);


(* ::Section:: *)
(*Insert Mode*)


(* ::Section:: *)
(*End*)


End[];
EndPackage[];
