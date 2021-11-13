{...............................................................................}
Var
    PCB_Board  : IPCB_Board;
    TIterator  : IPCB_BoardIterator;
    AIterator  : IPCB_BoardIterator;
    VIterator  : IPCB_BoardIterator;
    PIterator  : IPCB_BoardIterator;

    Track     : IPCB_Track;
    Arc       : IPCB_Arc;
    Via       : IPCB_Via;
    Net       : IPCB_Net;
    Pad       : IPCB_Pad;
    layer     : Tlayer;

    TrackList : TInterfaceList;
    X1List,Y1List,X2List,Y2List: TStringList; //�� ���������� ������� �� �����, ������� ����� ��������� �����
    NetName   : string;
    i,InitialSegmentIndex,NewSegmentIndex,CoordClick
              : integer;
    count     : integer;
{...............................................................................}

//�������� track, ������� ������. ������ ������� ��� ���� �� ����� ����� � ������� ��� ��������� ������� (track, arc, via) ������� ����������� � ���� ����.

{...............................................................................}

Function FindNextSegment(WorkIndex:integer):integer; //�������� ������� � ���� ��������, ���� ��� ����. ������� �� ������ ����� (����������), ���� ��� - �� ������� �����.
Begin

    CoordClick:=0;
    NewSegmentIndex:=-1;
    // �������� �� ���������� �1 � �1 (���) �  �1 � �1 (���)
    For i := 0 to TrackList.Count - 1 Do //�������� �� ���������� �
    Begin
      if (X1List[i] = X1List[WorkIndex]) and (TrackList.items[i].Selected=false) then //���� � �������...
      Begin
        if Y1List[i] = Y1List[WorkIndex] then  // ... � � �������, ������ �� ���� ����� ������������� ������� ����� �������.
        Begin
          CoordClick:=CoordClick+1; // ��������� �������, ���������� ������
          NewSegmentIndex:=i;
        End;
      End;
    End;

    // �������� �� ���������� �1 � �1 (���) �  �2 � �2 (���)
    For i := 0 to TrackList.Count - 1 Do //�������� �� ���������� �
    Begin
      if (X2List[i] = X1List[WorkIndex]) and (TrackList.items[i].Selected=false) then //���� � �������...
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
      TrackList.items[NewSegmentIndex].Selected:=true; TrackList.items[NewSegmentIndex].GraphicallyInvalidate;
      Result:=NewSegmentIndex;//���� ������� ����, ���������� ���. ���� �� ���/��� � ������, ���� ����� ��� �� ��������.
      Exit;
    End;


    //���� � ������� �����:

    CoordClick:=0;
    NewSegmentIndex:=-1;
    // �������� �� ���������� �2 � �2 (���) �  �1 � �1 (���)
    For i := 0 to TrackList.Count - 1 Do //�������� �� ���������� �
    Begin
      if (X1List[i] = X2List[WorkIndex]) and (TrackList.items[i].Selected=false) then //���� � �������...
      Begin
        if Y1List[i] = Y2List[WorkIndex] then  // ... � � �������, ������ �� ���� ����� ������������� ������� ����� �������.
        Begin
          CoordClick:=CoordClick+1; // ��������� �������, ���������� ������
          NewSegmentIndex:=i;
        End;
      End;
    End;

    // �������� �� ���������� �2 � �2 (���) �  �2 � �2 (���)
    For i := 0 to TrackList.Count - 1 Do //�������� �� ���������� �
    Begin
      if (X2List[i] = X2List[WorkIndex]) and (TrackList.items[i].Selected=false) then //���� � �������...
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
      TrackList.items[NewSegmentIndex].Selected:=true; TrackList.items[NewSegmentIndex].GraphicallyInvalidate;
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

    if  (PCB_Board.SelectecObjectCount > 1) then
    begin
         Client.SendMessage('PCB:SelectNext', 'SelectTopologyObjects = TRUE', 255, Client.CurrentView);
         Exit;
    end;

    Track := PCB_Board.SelectecObject[0];
    if (Track = Nil) then
       Track := PCB_Board.GetObjectAtCursor(MkSet(eTrackObject,eArcObject,eViaObject), AllLayers, 'Choose a Track');

    If Track <> Nil Then
    begin
      If Track.Net <> Nil Then
      begin
        Track.Selected:=true;
        Track.GraphicallyInvalidate;
        Net:=Track.Net;
        NetName:=Net.Name;
        layer:= Track.Layer;
      end;
    end else Exit;
    //�������� �������� ������� � ����� �������� ����



    TrackList := TInterfaceList.Create;
    X1List := TStringList.Create; Y1List := TStringList.Create; X2List := TStringList.Create; Y2List := TStringList.Create;

    TIterator  :=Net.GroupIterator_Create;
    TIterator.AddFilter_ObjectSet(MkSet(eTrackObject,eNoDimension,eCreate_Default));
    TIterator.AddFilter_LayerSet(MkSet(Layer)); ;
    //TIterator.AddFilter_Method(eProcessAll);

    AIterator  := Net.GroupIterator_Create;
    AIterator.AddFilter_ObjectSet(MkSet(eArcObject,eCreate_Default));
    AIterator.AddFilter_LayerSet(MkSet(Layer));
    //AIterator.AddFilter_Method(eProcessAll);

    VIterator  := Net.GroupIterator_Create;
    VIterator.AddFilter_ObjectSet(MkSet(eViaObject));
    VIterator.AddFilter_LayerSet(AllLayers);
    //VIterator.AddFilter_Method(eProcessAll);


    PIterator := Net.GroupIterator_Create;
    PIterator.AddFilter_ObjectSet(MkSet(ePadObject,eCreate_Default));
    PIterator.AddFilter_LayerSet(AllLayers);
    //PIterator.AddFilter_Method(eProcessAll);

    Track := TIterator.FirstPCBObject;
    Arc   := AIterator.FirstPCBObject;
    Via   := VIterator.FirstPCBObject;
    Pad   := PIterator.FirstPCBObject;

    While (Track <> Nil) Do
    Begin
      if Track.Net=Net then TrackList.Add(Track);
      Track := TIterator.NextPCBObject;
    End;

    While (Arc <> Nil) Do
    Begin
      if Arc.Net=Net then TrackList.Add(Arc);
      Arc := AIterator.NextPCBObject;
    End;

    While (Via <> Nil) Do
    Begin
      if Via.Net=Net then TrackList.Add(Via);
      Via := VIterator.NextPCBObject;
    End;

    While (Pad <> Nil) Do
    Begin
      if Pad.Net=Net then TrackList.Add(Pad);
      Pad := PIterator.NextPCBObject;
    End;

    Net.GroupIterator_Destroy(TIterator);
    Net.GroupIterator_Destroy(AIterator);
    //PCB_Board.BoardIterator_Destroy(TIterator);
    //PCB_Board.BoardIterator_Destroy(AIterator);
    Net.GroupIterator_Destroy(VIterator);
    Net.GroupIterator_Destroy(PIterator);
    //��������� ��� ��������, ������������� � ������ ���� � ������

    For i := 0 to TrackList.Count - 1 Do
    Begin

    if TrackList.items[i].Selected = true then InitialSegmentIndex:=i;

    Case TrackList.items[i].ObjectIDString of

        'Track': begin
                   Track:=TrackList.items[i];
                   X1List.Add(floattostr(coordtomms(Track.x1))); Y1List.Add(floattostr(coordtomms(Track.y1)));
                   X2List.Add(floattostr(coordtomms(Track.x2))); Y2List.Add(floattostr(coordtomms(Track.y2)));
                   //Track.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Track.Selected:=false;
                 end;

        'Arc'  : begin
                   Arc:=TrackList.items[i];
                   X1List.Add(floattostr( coordtomms(Arc.StartX) ));  Y1List.Add(floattostr( coordtomms(Arc.StartY) ));
                   X2List.Add(floattostr( coordtomms(Arc.EndX  ) ));  Y2List.Add(floattostr( coordtomms(Arc.EndY  ) ));
                   //Arc.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Arc.Selected:=false;
                 end;

        'Via'  : begin
                   Via:=TrackList.items[i];
                   X1List.Add(floattostr(coordtomms(Via.x))); Y1List.Add(floattostr(coordtomms(Via.y)));
                   X2List.Add(floattostr(coordtomms(Via.x))); Y2List.Add(floattostr(coordtomms(Via.y)));
                   //Via.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Via.Selected:=false;
                 end;

        'Pad'  : begin
                    Pad:=TrackList.items[i];
                    X1List.Add(floattostr(coordtomms(Pad.x))); Y1List.Add(floattostr(coordtomms(Pad.y)));
                    X2List.Add(floattostr(coordtomms(Pad.x))); Y2List.Add(floattostr(coordtomms(Pad.y)));
                    //Via.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Via.Selected:=false;
                 end;
    end;

    // �������� ��� ���������� ����� � ������ ��������� ���� (tracks,arcs,vias) � ��������� �������. ������ ����� ������ � ��� ���������� ����������.
    // ���� ����� ����� ���� ����������, ������ � ���� �����(�.�. �� ��������������� �������� i) ���� ��������.

    End;

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

    TrackList.Free; X1List.Free; Y1List.Free; X2List.Free; Y2List.Free;

    PCB_Board.GraphicallyInvalidate;
    Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView); // Update PCB document
End;
{...............................................................................}
