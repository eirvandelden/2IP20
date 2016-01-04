unit DisplayTree_Roy;

// In deze unit ontbreekt de code van de procedures AuxCalc en DrawTree.
// De plaatsen waar code toegevoegd moet geworden zijn gemarkeerd met TODO .

// Etienne van Delden
// 0618959


interface

uses
  Graphics;

var
   Rightmost: Integer; //we moeten voor AuxCalc weten wat de meest rechts node is die we hebben bekeken
   BDrawTrees: Boolean;

type
  TCompare = (ls, eq, gt);

  //----------------------------------------------------------------------------
  // A record of type TNode represents a node of a binary search tree.
  // In addition it contains layout information needed for drawing the tree.
  //----------------------------------------------------------------------------
  PNode = ^TNode;
  TNode =
  record
    // binary search tree fields
    FValue: Integer;
    FLeft: PNode;
    FRight: PNode;
    // layout fields
    FHeight: Integer; // height in pixels of the tree with this node as root.
    FWidth: Integer;  // width in pixels of the tree with this node as root.
    FX: Integer;      // relative X-xoordinate.
    FY: Integer;      // relative Y-coordinate.
  end;


  //----------------------------------------------------------------------------
  // The class TTree maintains a binary search tree (BST) with operations
  // Count, Occurs, Clear, Add and Remove. In addition, it also contains
  // facilities for calculating a layout for the tree and for drawing the tree
  // according to this layout.
  //----------------------------------------------------------------------------
  // Queries:
  // - Count           The numberof nodes in the tree
  // - ToString        A textual representation of the set of values in the tree
  // - Occurs(AValue)  Indicates whether a value occurs in the tree
  // - Width           Width in pixels of the tree drawing
  // - Height          Height in pixels of the tree drawing
  //-----------------------------
  // Binary search tree commands:
  // Clear             Remove all values from the tree
  // Add(AValue)       Add a value to the tree
  // Remove(AValue)    Delete a value from the tree
  //-----------------------------
  // Layout and draw commands:
  // - CalculateLayout(DrawEmptyTrees, RootPositionStyle)
  //                   Calculate the layout information for each node
  //     - DrawEmptyTrees:
  //                   Indicates whether empty trees should be drawn as well
  //     - RootPositionStyle:
  //                   Indicates where the root should br drawn w.r.t. subtrees:
  //       - 0: Midposition of width
  //       - 1: Halfway roots of subtrees
  //       - 2: Halfway gap between subtrees
  // - Draw(ACanvas, DrawBoundingBoxes)
  //                   Draws the tree on ACanvas. DrawBoundingBoxes indicates
  //                   whether bounding boxes should be drawn as well.                   
  //-----------------------------
  // Adjustable layout properties:
  // - HSpace          Horizontal spacing between adjacent trees
  // - VSpace          Vertical spacing between node positions
  // - NodeSize        *Half* the size of a node rectangle
  //----------------------------------------------------------------------------
  // FOR COMMENTS ON PROTECTED MEMBERS SEE THE IMPLEMENTATION SECTION
  //----------------------------------------------------------------------------
  TTree =
  class(TObject)
  protected
    FRoot: PNode;
    FHSpace: Integer;
    FVSpace: Integer;
    FNodeSize: Integer;
    FDrawBoundingBoxes: Boolean;
    FDrawEmptyTrees: Boolean;
    FRootPositionStyle: Integer;

    // auxiliary tree queries
    function Find(AValue: Integer; ANode: PNode): PNode;
    function CountNodes(ANode: PNode): Integer;
    function TreeToString(ANode: PNode): String;

    // auxiliary tree commands
    procedure DisposeTree(ANode: PNode);
    procedure Insert(AValue: Integer; var P: PNode);
    procedure Delete(AValue: Integer; var P: PNode);
    procedure DeleteRM(var R: PNode; var S: PNode);
    function TreeHeight(ANode: PNode): Integer;
    function TreeWidth(ANode: PNode): Integer;
    function TreeRootX(ANode: PNode): Integer;
    function TreeRootY(ANode: PNode): Integer;

    // auxiliary commands for layout computation and Drawing
    procedure AuxCalc(ANode: PNode);
    procedure DrawBackGround(ACanvas: TCanvas);
    procedure DrawTree(ANode: PNode; AX, AY: Integer; ACanvas: TCanvas);
  public
    // construction/destruction ---------------------------
    constructor Create;
    destructor Destroy; override;

    // queries --------------------------------------------
    function Count: Integer;
    function ToString: String;
    function Occurs(AValue: Integer): Boolean;
    function Width: Integer;
    function Height: Integer;

    // binary search tree commands ------------------------
    procedure Clear;
    procedure Add(AValue: Integer);
    procedure Remove(AValue: Integer);

    // layout and draw commands ---------------------------
    procedure CalculateLayout(DrawEmptyTrees: Boolean; RootPositionStyle: Integer);
    procedure Draw(ACanvas: TCanvas; DrawBoundingBoxes: Boolean);

    // adjustable layout parameters
    property HSpace: Integer read FHSpace write FHSpace; // horizontal spacing between adjacent trees
    property VSpace: Integer read FVSpace write FVSpace; // vertical spacing between node positions
    property NodeSize: integer read FNodeSize write FNodeSize; // *half* the size of a node rectangle
  end;

function Compare(X, Y: Integer): TCompare;
function MakeNode(AValue: Integer; ALeft, ARight: PNode): PNode;
// pre: true
// ret: pointer to a newly created TNode filled with the given parameter values

procedure ChangeDrawEmptyTrees(ATree:TTree; ABool: Boolean);



implementation //===============================================================

uses
  Math, SysUtils, Types, Dialogs;
{ TTree }

{TV: auxiliary function on PNode-trees}

function TTree.TreeHeight(ANode: PNode): Integer;
  { pre: ANode points to a binary search tree with up-to-date layout info
    ret: height of the tree in pixels }
begin
  if ANode = nil
  then Result := 0
  else Result := ANode^.FHeight
end;

function TTree.TreeWidth(ANode: PNode): Integer;
  { pre: ANode points to a binary search tree with up-to-date layout info
    ret: width of the tree in pixels }
begin
  if ANode = nil
  then if FDrawEmptyTrees then Result := 2*NodeSize else Result := 0
  else Result := ANode^.FWidth
end;

function TTree.TreeRootX(ANode: PNode): Integer;
  { pre: ANode points to a binary search tree with up-to-date layout info
    ret: relative X-coordinate of root }
begin
  if ANode = nil
  then if FDrawEmptyTrees then Result := NodeSize else Result := 0
  else Result := ANode^.FX
end;

function TTree.TreeRootY(ANode: PNode): Integer;
  { pre: ANode points to a binary search tree with up-to-date layout info
    ret: relative Y-coordinate of root }
begin
  if ANode = nil
  then Result := 0
  else Result := ANode^.FY
end;

procedure TTree.Add(AValue: Integer);
begin
  Insert(AValue, FRoot);
end;

procedure TTree.AuxCalc(ANode: PNode);
  { pre: ANode <> nil
    post: ANode and it's linked nodes have their layout data updated }
    
    {OWNCODEADDED}
    
var
  LWidth, RWidth: Integer; // Linked left and right node width
  LHeight, RHeight: Integer; // Linked left and right node height
  LX, RX: Integer; // Linked left and right X-positions
begin
  with ANode^ do begin
    // Make sure all linked nodes are calculated as well
    if FLeft <> nil then begin
      AuxCalc(FLeft); // Make sure left is fully calculated
      // Take some variables for convenience
      LWidth := FLeft^.FWidth;
      LHeight := FLeft^.FHeight;
      LX := FLeft^.FX;
    end else begin // When we have a nil pointer
      if FDrawEmptyTrees then // and need to draw the nil nodes
        LWidth := 2 * FNodeSize // accommodate for extra space
      else
        LWidth := 0; // or none if we don't
      LHeight := 0;
      LX := 0;
    end;
    // And do the same for right as we did for left
    if FRight <> nil then begin
      AuxCalc(FRight); // Make sure right is fully calculated
      // Take some variables for convenience
      RWidth := FRight^.FWidth;
      RHeight := FRight^.FHeight;
      RX := FRight^.FX;
    end else begin // When we have a nil pointer
      if FDrawEmptyTrees then // and need to draw the nil nodes
        RWidth := 2 * FNodeSize // accommodate for extra space
      else
        RWidth := 0; // or none if we don't
      RHeight := 0;
      RX := 0;
    end;

    // Assign width and height of curent node
    if (RWidth = 0) and (LWidth = 0) then begin // When we have only nil nodes,
      // and we don't draw those nil nodes
      FWidth := 2 * FNodeSize; // make it have a fixed width
      FHeight := 2 * FNodeSize; // and height (that of a single box)
    end else begin // When we don't, make sure all fits
      FWidth := Max(LWidth + FHSpace + RWidth, 2 * FNodeSize);
      FHeight := Max(LHeight, RHeight) + FVSpace + (2 * FNodeSize);
    end;

    // Position the current node (relatively)
    FY := FNodeSize; // Y-position is constant
    case FRootPositionStyle of
      0: FX := FWidth div 2; // Midposition of width
      1: begin // Halfway roots of subtrees
        if (RHeight = 0) and (LHeight = 0) then // When we have only nil's
          FX := FWidth div 2 // we will choose the middle
        else // or else use the standard method to get the proper FX
          FX := LX + ((LWidth + FHSpace + RX - LX) div 2);
      end;
      2: begin // Halfway gap between subtrees
        if (RWidth = 0) and (LWidth = 0) then // When facing undrawable nil's
          FX := FNodeSize // We must choose a constant FX
        else // When we do not,
          FX := LWidth + (FHSpace div 2); // it is safe to use the formula
      end;
    end; // case FRootPositionStyle
  end; // with ANode^
end;


procedure TTree.CalculateLayout(DrawEmptyTrees: Boolean; RootPositionStyle: Integer);
begin
  FDrawEmptyTrees := DrawEmptyTrees;
  FRootPositionStyle := RootPositionStyle;
  if FRoot <> nil
  then AuxCalc(FRoot);
end;

procedure TTree.Clear;
begin
  DisposeTree(FRoot);
  FRoot := nil;
end;

function TTree.Count: Integer;
begin
  Result := CountNodes(FRoot);
end;

function TTree.CountNodes(ANode: PNode): Integer;
// pre: ANode points to a binary search tree
// ret: the number of nodes in that tree
begin
  if ANode = nil
  then Result := 0
  else Result := 1 + CountNodes(ANode^.FLeft) + CountNodes(ANode^.FRight);
end;

constructor TTree.Create;
begin
  inherited Create;
  FRoot := nil;
end;

procedure TTree.Delete(AValue: Integer; var P: PNode);
// See Lecture Notes BST2.doc
var
  Q: PNode; // Node to be deleted
begin
  if P = nil
  then {skip}
  else
    case Compare(AValue, P^.FValue) of
      ls: Delete(AValue, P^.FLeft);
      gt: Delete(AValue, P^.FRight);
      eq:
        begin
          if P^.FRight = nil then begin Q := P; P := P^.FLeft end
          else if P^.FLeft = nil then begin Q := P; P := P^.FRight end
          else
            begin
              DeleteRM(P^.FLeft, Q);
              P^.FValue := Q^.FValue
            end;
          Dispose(Q)
        end;
    end{case}
end;

procedure TTree.DeleteRM(var R, S: PNode);
// See lecture Notes BST2.doc
// Make S refer to rightmost element of tree with root R;
// Remove that element from the tree
begin
  if R^.FRight = nil
  then begin S := R; R := S^.FLeft end
  else DeleteRM(R^.FRight, S);
end;

destructor TTree.Destroy;
begin
  DisposeTree(FRoot);
  inherited;
end;

procedure TTree.DisposeTree(ANode: PNode);
// pre: ANode points to the root of a binary search tree
// post: all nodes in the tree have ben disposed
begin
  if ANode <> nil then
  begin
    DisposeTree(ANode^.FLeft);
    DisposeTree(ANode^.FRight);
  end;
  Dispose(ANode);
end;

procedure TTree.DrawBackGround(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Brush.Color := clWhite;
    FillRect(ClipRect);
  end;
end;

procedure TTree.DrawTree(ANode: PNode; AX, AY: Integer; ACanvas: TCanvas);
  { pre: AX, AY are absolute coordinates of upper left-hand corner of
         the bounding box;
         ANode points to a binary search tree with up-to-date layout info
    post: ANode has been drawn on the canvas according to its layout info }
var
  ARect: TRect; // To store rectangle data before drawing
  ATX, ATY: Integer; // Temporary X and Y vars
  AVC: Integer; // Vertical constant (absolute position)
begin
  with ACanvas, ANode^ do begin
    if ANode = nil then begin // Draw a small line to indicate a nil node
      MoveTo(AX, AY);
      LineTo(AX + (2 * FNodeSize), AY); // of the width of a single block
      Exit; // and don't do anything else
    end;

    if FDrawBoundingBoxes then begin // Draw optional bounding boxes (trivial)
      ARect.Left := AX;
      ARect.Right := AX + FWidth;
      ARect.Top := AY;
      ARect.Bottom := AY + FHeight;
      Brush.Color := clBlack; // Border color
      FrameRect(ARect);
    end;

    AVC := AY + FVSpace + (2 * FNodeSize); // Used for code simplicity

    // Draw lines
    MoveTo(AX + FX, AY + FY); // From the node's middle
    if FLeft <> nil then begin
      LineTo(AX + FLeft^.FX, AVC + FLeft^.FY); // to the new node's middle
    end else if FDrawEmptyTrees then begin // or, if we draw nil nodes as well
      LineTo(AX + FNodeSize, AVC); // to the top of the nil node's place
    end;
    MoveTo(AX + FX, AY + FY); // From the node's middle
    if FRight <> nil then begin
      ATX := AX + FWidth - FRight^.FWidth + FRight^.FX;
      LineTo(ATX, AVC + FRight^.FY); // To the linked node's middle
    end else if FDrawEmptyTrees then begin
      ACanvas.LineTo(AX + FWidth - FNodeSize, AVC); // Or to the nil node's top
    end;

    // Draw all subnodes
    if (FLeft <> nil) or FDrawEmptyTrees then begin // When we should draw,
      DrawTree(FLeft, AX, AVC, ACanvas); // do so.
    end;
    if FRight <> nil then begin // We need the proper X for this
      ATX := AX + FWidth - FRight^.FWidth; // so we get it
      DrawTree(FRight, ATX, AVC, ACanvas);
    end else if FDrawEmptyTrees then begin // and this differs slightly
      ATX := AX + FWidth - (2 * FNodeSize); // as we can't read FRight's width
      DrawTree(FRight, ATX, AVC, ACanvas);
    end;

    // Draw boxes and add text (trivial)
    ARect.Left := AX + FX - FNodeSize;
    ARect.Right := AX + FX + FNodeSize;
    ARect.Top := AY + FY - FNodeSize;
    ARect.Bottom := AY + FY + FNodeSize;
    Pen.Color := clBlack; // Text color
    Brush.Color := clYellow; // Background color
    TextRect(ARect, ARect.Left + 3, ARect.Top + 3, IntToStr(ANode.FValue));
    Brush.Color := clBlack; // Border color
    FrameRect(ARect);
  end;
end;


procedure TTree.Draw(ACanvas: TCanvas; DrawBoundingBoxes: Boolean);
begin
  FDrawBoundingBoxes := DrawBoundingBoxes;
  DrawBackground(ACanvas);
  DrawTree(FRoot, 0, 0, ACanvas);
end;

function TTree.Find(AValue: Integer; ANode: PNode): PNode;
// See Lecture Notes BST2.doc
begin
  if ANode = nil
  then Result := nil
  else
    case Compare(AValue, ANode^.FValue) of
      ls: Result := Find(AValue, ANode^.FLeft);
      eq: Result := ANode;
      gt: Result := Find(AValue, ANode^.FRight);
    end{case};
end;

procedure TTree.Insert(AValue: Integer; var P: PNode);
// See Lecture Notes BST2.doc
begin
  if P = nil
  then P := MakeNode(AValue, nil, nil)
  else
    case Compare(AValue, P^.FValue) of
      ls: Insert(AValue, P^.FLeft);
      eq: {AValue already present; do not insert again};
      gt: Insert(AValue, P^.FRight)
    end{case};
end;

function TTree.Occurs(AValue: Integer): Boolean;
begin
  Result := Find(AValue, FRoot) <> nil;
end;

procedure TTree.Remove(AValue: Integer);
begin
  Delete(AValue, FRoot);
end;

function TTree.ToString: String;
begin
  Result := TreeToString(FRoot);
end;

function Compare(X, Y: Integer): TCompare;
begin
  if X < Y
  then Result := ls
  else if X = Y then Result := eq
  else Result := gt;
end;

function MakeNode(AValue: Integer; ALeft, ARight: PNode): PNode;
var
  H: PNode;
  I: PNode;
begin
  New(H);
  with H^ do
  begin
    FValue := AValue;
    FLeft := ALeft;
    FRight := ARight;

    FHeight := 0;
    FWidth := 0;
    FX := 0;
    FY := 0;
  end{with};
  Result := H;
end;


function TTree.Height: Integer;
begin
  if FRoot <> nil
  then Result := FRoot^.FHeight
  else Result := 0;
end;

function TTree.Width: Integer;
begin
  if FRoot <> nil
  then Result := FRoot^.FWidth
  else Result := 0;
end;

function TTree.TreeToString(ANode: PNode): String;
begin
  if FRoot = nil
  then Result := ''
  else Result := TreeToString(ANode^.FLeft) + '  ' + IntToStr(ANode^.FValue) +
                   '  ' + TreeToString(ANode^.FRight);
end;


procedure ChangeDrawEmptyTrees(ATree:TTree; ABool: Boolean);

begin
  ATree.FDrawEmptyTrees :=  ABool;
  BDrawTrees := ABool;

end;

end.
