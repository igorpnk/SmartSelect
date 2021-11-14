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
    X1List,Y1List,X2List,Y2List: TStringList; //ну неизвестно сколько их будет, поэтому одним двумерным никак
    i,InitialSegmentIndex,NewSegmentIndex,CoordClick
              : integer;
    count     : integer;

    Block     : Boolean;
{...............................................................................}

//выделить track, вызвать скрипт. Скрипт выделит имя цепи из этого трэка и выделит все остальные объекты (track, arc, via) которые принадлежат к этой цепи.

{...............................................................................}

Function FindNextSegment(WorkIndex:integer):integer; //получает сегмент и ищет соседние, если они одни. сначала по одному концу (возвращает), если нет - по второму концу.
Begin

    CoordClick:=0;
    NewSegmentIndex:=-1;

    // проверка на совпадение Х1 и У1 (исх) и  Х1 и У1 (нов)
    For i := 0 to PrimList.Count - 1 Do //проверка на совпадение Х
    Begin
      if (X1List[i] = X1List[WorkIndex]) and (PrimList.items[i].Selected=false) then //если Х совпало...
      Begin
        if  (Y1List[i] = Y1List[WorkIndex]) then  // ... и У совпало, значит из этой точки действительно выходит новый сегмент.
        Begin
          CoordClick:=CoordClick+1; // добавляем счетчик, запоминаем индекс
          NewSegmentIndex:=i;
        End;
      End;
    End;

    // проверка на совпадение Х1 и У1 (исх) и  Х2 и У2 (нов)
    For i := 0 to PrimList.Count - 1 Do //проверка на совпадение Х
    Begin
      if (X2List[i] = X1List[WorkIndex])  and (PrimList.items[i].Selected=false) then //если Х совпало...
      Begin
        if ((Y2List[i] = Y1List[WorkIndex])) then  // ... и У совпало, значит из этой точки действительно выходит новый сегмент.
        Begin
          CoordClick:=CoordClick+1; // добавляем счетчик, запоминаем индекс
          NewSegmentIndex:=i;
        End;
      End;
    End;

    if CoordClick=1 Then
    Begin
      PrimList.items[NewSegmentIndex].Selected:=true; PrimList.items[NewSegmentIndex].GraphicallyInvalidate;
      Result:=NewSegmentIndex;//если сегмент один, возвращаем его. Если их нет/два и больше, этот конец нам не подходит.
      Exit;
    End;


    //Ищем с другого конца:

    CoordClick:=0;
    NewSegmentIndex:=-1;
    // проверка на совпадение Х2 и У2 (исх) и  Х1 и У1 (нов)
    For i := 0 to PrimList.Count - 1 Do //проверка на совпадение Х
    Begin
      if (X1List[i] = X2List[WorkIndex]) and (PrimList.items[i].Selected=false) then //если Х совпало...
      Begin
        if (Y1List[i] = Y2List[WorkIndex]) then  // ... и У совпало, значит из этой точки действительно выходит новый сегмент.
        Begin
          CoordClick:=CoordClick+1; // добавляем счетчик, запоминаем индекс
          NewSegmentIndex:=i;
        End;
      End;
    End;

    // проверка на совпадение Х2 и У2 (исх) и  Х2 и У2 (нов)
    For i := 0 to PrimList.Count - 1 Do //проверка на совпадение Х
    Begin
      if (X2List[i] = X2List[WorkIndex]) and (PrimList.items[i].Selected=false) then //если Х совпало...
      Begin
        if (Y2List[i] = Y2List[WorkIndex]) then  // ... и У совпало, значит из этой точки действительно выходит новый сегмент.
        Begin
          CoordClick:=CoordClick+1; // добавляем счетчик, запоминаем индекс
          NewSegmentIndex:=i;
        End;
      End;
    End;

    if CoordClick=1 Then
    Begin
      PrimList.items[NewSegmentIndex].Selected:=true; PrimList.items[NewSegmentIndex].GraphicallyInvalidate;
      Result:=NewSegmentIndex;//если сегмент один, возвращаем его. Если их нет/два и больше, этот конец нам не подходит.
      Exit;
    End;

    NewSegmentIndex:=-1;
    Result:=WorkIndex; // если и справа и слева развилка/уже выделенные сегменты, возвращаем исходный.
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
         while  (PCB_Board.SelectecObject[1] <> NIL) do  //убираем выделение со всех объектов кроме нулевого.
         begin
            PCB_Board.SelectecObject[1].Selected := false;
         end;

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
    //выделили исходный сегмент и взяли название цепи

    PrimList := TInterfaceList.Create;
    X1List := TStringList.Create; Y1List := TStringList.Create; X2List := TStringList.Create; Y2List := TStringList.Create;

    Iterator  :=Net.GroupIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eTrackObject,eArcObject,eViaObject,ePadObject,eNoDimension,eCreate_Default));
    if (layer = eMultiLayer) then  Iterator.AddFilter_LayerSet(AllLayers);
    if (layer <> eMultiLayer) then Iterator.AddFilter_LayerSet(MkSet(Layer,eMultiLayer));

    Primitive := Iterator.FirstPCBObject;

    While (Primitive <> Nil) Do
    Begin
      if Primitive.Net=Net then PrimList.Add(Primitive);
      Primitive := Iterator.NextPCBObject;
    End;

    Net.GroupIterator_Destroy(Iterator);

    For i := 0 to PrimList.Count - 1 Do
    Begin

    if PrimList.items[i].Selected = true then InitialSegmentIndex:=i;

    Case PrimList.items[i].ObjectIDString of

        'Track': begin
                   Track:=PrimList.items[i];
                   X1List.Add(IntToStr(Track.x1)); Y1List.Add(IntToStr(Track.y1));
                   X2List.Add(IntToStr(Track.x2)); Y2List.Add(IntToStr(Track.y2));
                   //Track.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Track.Selected:=false;
                 end;

        'Arc'  : begin
                   Arc:=PrimList.items[i];
                   X1List.Add(IntToStr( Arc.StartX ));  Y1List.Add(IntToStr( Arc.StartY ));
                   X2List.Add(IntToStr( Arc.EndX  ));  Y2List.Add(IntToStr( Arc.EndY  ));
                   //Arc.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Arc.Selected:=false;
                 end;

        'Via'  : begin
                   Via:=PrimList.items[i];
                   X1List.Add(IntToStr(Via.x)); Y1List.Add(IntToStr(Via.y));
                   X2List.Add(IntToStr(Via.x)); Y2List.Add(IntToStr(Via.y));
                   //Via.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Via.Selected:=false;
                 end;

        'Pad'  : begin
                    Pad:=PrimList.items[i];
                    X1List.Add(IntToStr(Pad.x)); Y1List.Add(IntToStr(Pad.y));
                    X2List.Add(IntToStr(Pad.x)); Y2List.Add(IntToStr(Pad.y));
                    //Via.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Via.Selected:=false;
                 end;
    end;

    // выписали все координаты начал и концов сегментов цепи (tracks,arcs,vias) в отдельные массивы. Теперь можно искать в них одинаковые координаты.
    // Если найдём более двух совпадений, значит в этом месте(т.е. на соответствующем элементе i) есть развилка.

    End;

    Block := false;

    FindNextSegment(InitialSegmentIndex); // он пойдёт в одну сторону и упрется

    While (NewSegmentIndex<>-1) Do
    Begin
      FindNextSegment(NewSegmentIndex);
    End;

    FindNextSegment(InitialSegmentIndex);  // поэтому вызываем всё это to ераз, чтобы он прошел еще и в другую сторону

    While (NewSegmentIndex<>-1) Do
    Begin
      FindNextSegment(NewSegmentIndex);
    End;

    if (PCB_Board.SelectecObjectCount = 1) then   //если ничего не выделилось, то возможно у нас Via, pad или один сегмент
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
