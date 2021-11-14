{...............................................................................}
Var
    PCB_Board  : IPCB_Board;
    Iterator  : IPCB_GroupIterator;

    Track     : IPCB_Track;
    Arc       : IPCB_Arc;
    Via       : IPCB_Via;
    Net       : IPCB_Net;
    Pad       : IPCB_Pad;
    layer     : Tlayer;
    Primitive : IPCB_Primitive;

    PrimList : TInterfaceList;
    X1List,Y1List,X2List,Y2List: TStringList; //�� ���������� ������� �� �����, ������� ����� ��������� �����
    i,InitialSegmentIndex,NewSegmentIndex,CoordClick
              : integer;
    count     : integer;

    Block     : Boolean;
{...............................................................................}

//�������� track, ������� ������. ������ ������� ��� ���� �� ����� ����� � ������� ��� ��������� ������� (track, arc, via) ������� ����������� � ���� ����.

{...............................................................................}

Function FindNextSegment(WorkIndex:integer):integer; //�������� ������� � ���� ��������, ���� ��� ����. ������� �� ������ ����� (����������), ���� ��� - �� ������� �����.
Begin

    CoordClick:=0;
    NewSegmentIndex:=-1;
    // �������� �� ���������� �1 � �1 (���) �  �1 � �1 (���)
    For i := 0 to PrimList.Count - 1 Do //�������� �� ���������� �
    Begin
      if (X1List[i] = X1List[WorkIndex]) and (PrimList.items[i].Selected=false) then //���� � �������...
      Begin
        if Y1List[i] = Y1List[WorkIndex] then  // ... � � �������, ������ �� ���� ����� ������������� ������� ����� �������.
        Begin
          CoordClick:=CoordClick+1; // ��������� �������, ���������� ������
          NewSegmentIndex:=i;
        End;
      End;
    End;

    // �������� �� ���������� �1 � �1 (���) �  �2 � �2 (���)
    For i := 0 to PrimList.Count - 1 Do //�������� �� ���������� �
    Begin
      if (X2List[i] = X1List[WorkIndex]) and (PrimList.items[i].Selected=false) then //���� � �������...
      Begin
        if Y2List[i] = Y1List[WorkIndex] then  // ... � � �������, ������ �� ���� ����� ������������� ������� ����� �������.
        Begin
          CoordClick:=CoordClick+1; // ��������� �������, ���������� ������
          NewSegmentIndex:=i;
        End;
      End;
    End;

    if CoordClick=1 Then
    Begin
      PrimList.items[NewSegmentIndex].Selected:=true; PrimList.items[NewSegmentIndex].GraphicallyInvalidate;
      Result:=NewSegmentIndex;//���� ������� ����, ���������� ���. ���� �� ���/��� � ������, ���� ����� ��� �� ��������.
      Exit;
    End;


    //���� � ������� �����:

    CoordClick:=0;
    NewSegmentIndex:=-1;
    // �������� �� ���������� �2 � �2 (���) �  �1 � �1 (���)
    For i := 0 to PrimList.Count - 1 Do //�������� �� ���������� �
    Begin
      if (X1List[i] = X2List[WorkIndex]) and (PrimList.items[i].Selected=false) then //���� � �������...
      Begin
        if Y1List[i] = Y2List[WorkIndex] then  // ... � � �������, ������ �� ���� ����� ������������� ������� ����� �������.
        Begin
          CoordClick:=CoordClick+1; // ��������� �������, ���������� ������
          NewSegmentIndex:=i;
        End;
      End;
    End;

    // �������� �� ���������� �2 � �2 (���) �  �2 � �2 (���)
    For i := 0 to PrimList.Count - 1 Do //�������� �� ���������� �
    Begin
      if (X2List[i] = X2List[WorkIndex]) and (PrimList.items[i].Selected=false) then //���� � �������...
      Begin
        if Y2List[i] = Y2List[WorkIndex] then  // ... � � �������, ������ �� ���� ����� ������������� ������� ����� �������.
        Begin
          CoordClick:=CoordClick+1; // ��������� �������, ���������� ������
          NewSegmentIndex:=i;
        End;
      End;
    End;

    if CoordClick=1 Then
    Begin
      PrimList.items[NewSegmentIndex].Selected:=true; PrimList.items[NewSegmentIndex].GraphicallyInvalidate;
      Result:=NewSegmentIndex;//���� ������� ����, ���������� ���. ���� �� ���/��� � ������, ���� ����� ��� �� ��������.
      Exit;
    End;

    NewSegmentIndex:=-1;
    Result:=WorkIndex; // ���� � ������ � ����� ��������/��� ���������� ��������, ���������� ��������.
End;

Procedure SelectNetSegment;
Begin

    PCB_Board       := PCBServer.GetCurrentPCBBoard;
    If PCB_Board     = Nil Then Exit;


    if (PCB_Board.SelectecObject[0] = NIL) then
    begin
         Primitive := PCB_Board.GetObjectAtCursor(MkSet(eTrackObject,eArcObject,eViaObject,ePadObject), AllLayers, 'Choose a Track');
    end  else
    begin

    if  (PCB_Board.SelectecObjectCount > 1) then
    begin
         Client.SendMessage('PCB:SelectNext', 'SelectTopologyObjects = TRUE', 255, Client.CurrentView);
         Exit;
    end;

    if  ((PCB_Board.SelectecObject[0].ObjectID  = eViaObject) or (PCB_Board.SelectecObject[0].ObjectID  = ePadObject))  then
    begin
         //Client.SendMessage('PCB:SelectNext', 'SelectTopologyObjects = TRUE', 255, Client.CurrentView);
         //Exit;
    end;

    if ((PCB_Board.SelectecObject[0].ObjectID  = eTrackObject) or (PCB_Board.SelectecObject[0].ObjectID  = eArcObject)
       or (PCB_Board.SelectecObject[0].ObjectID  = eViaObject) or (PCB_Board.SelectecObject[0].ObjectID  = ePadObject))
       then
           begin
                Primitive := PCB_Board.SelectecObject[0];
           end
       else
           begin
                Exit;
           end;
    end;


    If Primitive <> Nil Then
    begin
      If Primitive.Net <> Nil Then
      begin
        Primitive.Selected:=true;
        Primitive.GraphicallyInvalidate;
        Net:=Primitive.Net;
        layer:= Primitive.Layer;
      end else
      begin
      Client.SendMessage('PCB:SelectNext', 'SelectTopologyObjects = TRUE', 255, Client.CurrentView);
         Exit;
      end;

    end else Exit;
    //�������� �������� ������� � ����� �������� ����

    PrimList := TInterfaceList.Create;
    X1List := TStringList.Create; Y1List := TStringList.Create; X2List := TStringList.Create; Y2List := TStringList.Create;

    Iterator  :=Net.GroupIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eTrackObject,eArcObject,eViaObject,ePadObject,eNoDimension,eCreate_Default));
    if (layer = eMultiLayer) then  Iterator.AddFilter_LayerSet(AllLayers);
    if (layer <> eMultiLayer) then Iterator.AddFilter_LayerSet(MkSet(Layer,eMultiLayer));
    //TIterator.AddFilter_Method(eProcessAll);

    // AIterator  := Net.GroupIterator_Create;
    //AIterator.AddFilter_ObjectSet(MkSet(eArcObject,eCreate_Default));
    //AIterator.AddFilter_LayerSet(MkSet(Layer));
    //AIterator.AddFilter_Method(eProcessAll);

    //VIterator  := Net.GroupIterator_Create;
    //VIterator.AddFilter_ObjectSet(MkSet(eViaObject));
   // VIterator.AddFilter_LayerSet(AllLayers);
    //VIterator.AddFilter_Method(eProcessAll);


    //PIterator := Net.GroupIterator_Create;
    //PIterator.AddFilter_ObjectSet(MkSet(ePadObject,eCreate_Default));
    //PIterator.AddFilter_LayerSet(AllLayers);
    //PIterator.AddFilter_Method(eProcessAll);

    Primitive := Iterator.FirstPCBObject;
    //Arc   := AIterator.FirstPCBObject;
    //Via   := VIterator.FirstPCBObject;
    //Pad   := PIterator.FirstPCBObject;

    While (Primitive <> Nil) Do
    Begin
      if Primitive.Net=Net then PrimList.Add(Primitive);
      Primitive := Iterator.NextPCBObject;
    End;

    //While (Arc <> Nil) Do
    //Begin
    //  if Arc.Net=Net then PrimList.Add(Arc);
    //  Arc := AIterator.NextPCBObject;
    //End;

    //While (Via <> Nil) Do
    //Begin
    //  if Via.Net=Net then PrimList.Add(Via);
    //  Via := VIterator.NextPCBObject;
    //End;

    //While (Pad <> Nil) Do
    //Begin
    //  if Pad.Net=Net then PrimList.Add(Pad);
    //  Pad := PIterator.NextPCBObject;
    //End;

    Net.GroupIterator_Destroy(Iterator);
    //Net.GroupIterator_Destroy(AIterator);
    //PCB_Board.BoardIterator_Destroy(TIterator);
    //PCB_Board.BoardIterator_Destroy(AIterator);
    //Net.GroupIterator_Destroy(VIterator);
    //Net.GroupIterator_Destroy(PIterator);
    //��������� ��� ��������, ������������� � ������ ���� � ������

    For i := 0 to PrimList.Count - 1 Do
    Begin

    if PrimList.items[i].Selected = true then InitialSegmentIndex:=i;

    Case PrimList.items[i].ObjectIDString of

        'Track': begin
                   Track:=PrimList.items[i];
                   X1List.Add(floattostr(coordtomms(Track.x1))); Y1List.Add(floattostr(coordtomms(Track.y1)));
                   X2List.Add(floattostr(coordtomms(Track.x2))); Y2List.Add(floattostr(coordtomms(Track.y2)));
                   //Track.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Track.Selected:=false;
                 end;

        'Arc'  : begin
                   Arc:=PrimList.items[i];
                   X1List.Add(floattostr( coordtomms(Arc.StartX) ));  Y1List.Add(floattostr( coordtomms(Arc.StartY) ));
                   X2List.Add(floattostr( coordtomms(Arc.EndX  ) ));  Y2List.Add(floattostr( coordtomms(Arc.EndY  ) ));
                   //Arc.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Arc.Selected:=false;
                 end;

        'Via'  : begin
                   Via:=PrimList.items[i];
                   X1List.Add(floattostr(coordtomms(Via.x))); Y1List.Add(floattostr(coordtomms(Via.y)));
                   X2List.Add(floattostr(coordtomms(Via.x))); Y2List.Add(floattostr(coordtomms(Via.y)));
                   //Via.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Via.Selected:=false;
                 end;

        'Pad'  : begin
                    Pad:=PrimList.items[i];
                    X1List.Add(floattostr(coordtomms(Pad.x))); Y1List.Add(floattostr(coordtomms(Pad.y)));
                    X2List.Add(floattostr(coordtomms(Pad.x))); Y2List.Add(floattostr(coordtomms(Pad.y)));
                    //Via.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Via.Selected:=false;
                 end;
    end;

    // �������� ��� ���������� ����� � ������ ��������� ���� (tracks,arcs,vias) � ��������� �������. ������ ����� ������ � ��� ���������� ����������.
    // ���� ����� ����� ���� ����������, ������ � ���� �����(�.�. �� ��������������� �������� i) ���� ��������.

    End;

    Block := false;

    FindNextSegment(InitialSegmentIndex); // �� ����� � ���� ������� � �������

    While (NewSegmentIndex<>-1) Do
    Begin
      FindNextSegment(NewSegmentIndex);
    End;

    FindNextSegment(InitialSegmentIndex);  // ������� �������� �� ��� to ����, ����� �� ������ ��� � � ������ �������

    While (NewSegmentIndex<>-1) Do
    Begin
      FindNextSegment(NewSegmentIndex);
    End;

    if (PCB_Board.SelectecObjectCount = 1) then   //���� ������ �� ����������, �� �������� � ��� Via, pad ��� ���� �������
     begin
          Block := true;
          Client.SendMessage('PCB:SelectNext', 'SelectTopologyObjects = TRUE', 255, Client.CurrentView);
          Exit;
     end;

    PrimList.Free; X1List.Free; Y1List.Free; X2List.Free; Y2List.Free;

    PCB_Board.GraphicallyInvalidate;
    Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView); // Update PCB document
End;
{...............................................................................}
