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
    X1List,Y1List,X2List,Y2List: TList; //ну неизвестно сколько их будет, поэтому одним двумерным никак
    i,InitialSegmentIndex,NewSegmentIndex,CoordClick
              : integer;
    count     : integer;

    Block     : Boolean;
    segments  : boolean;
    OldBlock  : Boolean;
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
      if ((X1List[i] = X1List[WorkIndex]) or (X1List[i] = X1List[WorkIndex]+1) or (X1List[i] = X1List[WorkIndex]-1)) and (PrimList.items[i].Selected=false) then //если Х совпало...
      Begin
        if  ((Y1List[i] = Y1List[WorkIndex]) or (Y1List[i] = Y1List[WorkIndex]+1) or (Y1List[i] = Y1List[WorkIndex]-1) ) then  // ... и У совпало, значит из этой точки действительно выходит новый сегмент.
        Begin
          CoordClick:=CoordClick+1; // добавляем счетчик, запоминаем индекс
          NewSegmentIndex:=i;
        End;
      End;
    End;

    // проверка на совпадение Х1 и У1 (исх) и  Х2 и У2 (нов)
    For i := 0 to PrimList.Count - 1 Do //проверка на совпадение Х
    Begin
      if ((X2List[i] = X1List[WorkIndex]) or (X2List[i] = X1List[WorkIndex]+1) or (X2List[i] = X1List[WorkIndex]-1))  and (PrimList.items[i].Selected=false) then //если Х совпало...
      Begin
        if ((Y2List[i] = Y1List[WorkIndex]) or (Y2List[i] = Y1List[WorkIndex]+1) or (Y2List[i] = Y1List[WorkIndex] -1)) then  // ... и У совпало, значит из этой точки действительно выходит новый сегмент.
        Begin
          CoordClick:=CoordClick+1; // добавляем счетчик, запоминаем индекс
          NewSegmentIndex:=i;
        End;
      End;
    End;


    if (NewSegmentIndex <> -1) then
    if (((CoordClick=1) or (block)))  Then
    Begin

      if ((block = false) and (OldBlock = true))
      then
      begin
                 if (((X1List[NewSegmentIndex] <> X1List[InitialSegmentIndex]) and
                      (Y1List[NewSegmentIndex] <> Y1List[InitialSegmentIndex])) or
                     ((X2List[NewSegmentIndex] <> X2List[InitialSegmentIndex]) and
                      (Y2List[NewSegmentIndex] <> Y2List[InitialSegmentIndex])))
                 then
                 begin
                 PrimList.items[NewSegmentIndex].Selected:=true; PrimList.items[NewSegmentIndex].GraphicallyInvalidate;
                 Result:=NewSegmentIndex;//если сегмент один, возвращаем его. Если их нет/два и больше, этот конец нам не подходит.
                 Exit;
                 end;
      end
      else
      begin
                 PrimList.items[NewSegmentIndex].Selected:=true; PrimList.items[NewSegmentIndex].GraphicallyInvalidate;
                 Result:=NewSegmentIndex;//если сегмент один, возвращаем его. Если их нет/два и больше, этот конец нам не подходит.
                 Exit;
      end;

    End else
    begin
      segments := true;
    end;


    //Ищем с другого конца:

    CoordClick:=0;
    NewSegmentIndex:=-1;
    // проверка на совпадение Х2 и У2 (исх) и  Х1 и У1 (нов)
    For i := 0 to PrimList.Count - 1 Do //проверка на совпадение Х
    Begin
      if ((X1List[i] = X2List[WorkIndex]) or (X1List[i] = X2List[WorkIndex] +1) or (X1List[i] = X2List[WorkIndex]-1)) and (PrimList.items[i].Selected=false) then //если Х совпало...
      Begin
        if ((Y1List[i] = Y2List[WorkIndex]) or (Y1List[i] = Y2List[WorkIndex]+1) or (Y1List[i] = Y2List[WorkIndex]-1)) then  // ... и У совпало, значит из этой точки действительно выходит новый сегмент.
        Begin
          CoordClick:=CoordClick+1; // добавляем счетчик, запоминаем индекс
          NewSegmentIndex:=i;
        End;
      End;
    End;

    // проверка на совпадение Х2 и У2 (исх) и  Х2 и У2 (нов)
    For i := 0 to PrimList.Count - 1 Do //проверка на совпадение Х
    Begin
      if ((X2List[i] = X2List[WorkIndex]) or (X2List[i] = X2List[WorkIndex]+1) or (X2List[i] = X2List[WorkIndex]-1)) and (PrimList.items[i].Selected=false) then //если Х совпало...
      Begin
        if ((Y2List[i] = Y2List[WorkIndex]) or (Y2List[i] = Y2List[WorkIndex]+1) or (Y2List[i] = Y2List[WorkIndex]-1)) then  // ... и У совпало, значит из этой точки действительно выходит новый сегмент.
        Begin
          CoordClick:=CoordClick+1; // добавляем счетчик, запоминаем индекс
          NewSegmentIndex:=i;
        End;
      End;
    End;

    if ((CoordClick=1) or (block)) Then
    Begin
      if (NewSegmentIndex = -1) then
      begin
         exit;
      end;
      PrimList.items[NewSegmentIndex].Selected:=true; PrimList.items[NewSegmentIndex].GraphicallyInvalidate;
      Result:=NewSegmentIndex;//если сегмент один, возвращаем его. Если их нет/два и больше, этот конец нам не подходит.
      Exit;
    End else
    begin
      segments := true;
    end;

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
    X1List := TList.Create; Y1List := TList.Create; X2List := TList.Create; Y2List := TList.Create;

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
                   X1List.Add((Track.x1)); Y1List.Add((Track.y1));
                   X2List.Add((Track.x2)); Y2List.Add((Track.y2));
                   //Track.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Track.Selected:=false;
                 end;

        'Arc'  : begin
                   Arc:=PrimList.items[i];
                   X1List.Add(( Arc.StartX ));  Y1List.Add(( Arc.StartY ));
                   X2List.Add(( Arc.EndX  ));  Y2List.Add(( Arc.EndY  ));
                   //Arc.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Arc.Selected:=false;
                 end;

        'Via'  : begin
                   Via:=PrimList.items[i];
                   X1List.Add((Via.x)); Y1List.Add((Via.y));
                   X2List.Add((Via.x)); Y2List.Add((Via.y));
                   //Via.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Via.Selected:=false;
                 end;

        'Pad'  : begin
                    Pad:=PrimList.items[i];
                    X1List.Add((Pad.x)); Y1List.Add((Pad.y));
                    X2List.Add((Pad.x)); Y2List.Add((Pad.y));
                    //Via.Selected:=true; ShowInfo('x1: '+X1List[i]+'; y1: '+Y1List[i]+'; x2: '+X2List[i]+'; y2: '+Y2List[i]); Via.Selected:=false;
                 end;
    end;

    // выписали все координаты начал и концов сегментов цепи (tracks,arcs,vias) в отдельные массивы. Теперь можно искать в них одинаковые координаты.
    // Если найдём более двух совпадений, значит в этом месте(т.е. на соответствующем элементе i) есть развилка.

    End;

    Block := false;
    OldBlock := false;

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
          //Client.SendMessage('PCB:SelectNext', 'SelectTopologyObjects = TRUE', 255, Client.CurrentView);
          //Exit;
          Segments := true;


          While (Segments) do
          begin
             Segments := false;
             Block := true;
             OldBlock := true;
             FindNextSegment(InitialSegmentIndex);
             Block := false;

             While (NewSegmentIndex<>-1) Do
             Begin
               FindNextSegment(NewSegmentIndex);
             End;

          end;
     end;

    PrimList.Free; X1List.Free; Y1List.Free; X2List.Free; Y2List.Free;

    PCB_Board.GraphicallyInvalidate;
    Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView); // Update PCB document
End;
{...............................................................................}
