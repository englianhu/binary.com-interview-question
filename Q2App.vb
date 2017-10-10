Rem Attribute VBA_ModuleType=VBAModule
Option VBASupport 1
Option Explicit

Public NewCustomerGraph As Boolean

Public Sub CustomerGraph()
'
' CustomerGraph Macro
' Macro recorded 4/1/2001 by Michael Thomas

    Dim i As Integer
    Dim ColCount As Integer
    Dim Servers As Integer
    
    Worksheets("Simulation").Unprotect
    Worksheets("Computations").Unprotect
    
    NewCustomerGraph = True
    
    ColCount = 0
    i = 1
    
    'Do While Worksheets("Computations").Cells(8, i) <> ""
    '    ColCount = ColCount + 1
    '    i = i + 1
    'Loop
    
    Do While Worksheets("Simulation").Cells(8, i) <> ""
        ColCount = ColCount + 1
        i = i + 1
    Loop
    Servers = (ColCount - 11) / 2
    
        'calculates how many columns are in 'Computations from the 'Simulation' sheet
    ColCount = ColCount - Servers + 3
    
    Worksheets("Computations").Cells(7, ColCount + 1) = "Service"
    Worksheets("Computations").Cells(8, ColCount + 1) = "Time"
    Worksheets("Computations").Cells(9, ColCount + 1) = "(with Departures)"
        'centres the "Service Time with Departures" column added to Computations
    Worksheets("Computations").Select
    Worksheets("Computations").Range("" & GetColumnLetter(ColCount + 1) & "7:" & GetColumnLetter(ColCount + 1) & "26").Select
    Selection.HorizontalAlignment = xlCenter
    
    Worksheets("Computations").Cells(7, ColCount + 2) = "Aggregate"
    Worksheets("Computations").Cells(8, ColCount + 2) = "Wait Time"
    
    For i = 12 To 26
            'Service Time
        'Worksheets("Computations").Cells(i, ColCount + 1) = "=IF(H" & i & "=""renege"","""",E" & i & ")"
        Worksheets("Computations").Cells(i, ColCount + 1) = "=IF(OR(H" & i & "=""balk""," & GetColumnLetter(Servers + 11) & "" & i & "=""renege""),"""",E" & i & ")"
            'Aggregate Wait Time
        Worksheets("Computations").Cells(i, ColCount + 2) = "=IF(Simulation!" & GetColumnLetter(6) & "" & i & "=""renege"",Simulation!" & GetColumnLetter(2 * Servers + 9) & "" & i & ",Simulation!" & GetColumnLetter(2 * Servers + 10) & "" & i & ")"
        Next i
    
        'formats the "renegeing" Service Time to time format
    Worksheets("Computations").Range("" & GetColumnLetter(ColCount + 1) & "12:" & GetColumnLetter(ColCount + 2) & "26").Select
    Selection.NumberFormat = "h:mm"
    
    Worksheets("Simulation").Select
'_________________________________________________________________________________
    
    Charts.Add
    ActiveChart.Location Where:=xlLocationAsObject, Name:="Simulation"
    ActiveChart.ChartType = xlBarStacked
    ActiveChart.SeriesCollection.NewSeries
    ActiveChart.SeriesCollection.NewSeries
    ActiveChart.SeriesCollection.NewSeries
    
    'Deletes extra Serieses if they are automatically inserted into the chart
    If ActiveChart.SeriesCollection.Count > 3 Then
        For i = 4 To ActiveChart.SeriesCollection.Count
            ActiveChart.SeriesCollection(i).Delete
        Next i
    End If
    
    ActiveChart.SeriesCollection(1).Values = "=Computations!R12C6:R26C6"
    ActiveChart.SeriesCollection(1).Name = "="""""
    'ActiveChart.SeriesCollection(2).Values = "=Simulation!R12C" & ColCount + Servers - 3 & ":R26C" & ColCount + Servers - 3 & ""
    ActiveChart.SeriesCollection(2).Values = "=Computations!R12C" & ColCount + 2 & ":R26C" & ColCount + 2 & ""
    ActiveChart.SeriesCollection(2).Name = "=""Time in Queue"""
    ActiveChart.SeriesCollection(3).Values = "=Computations!R12C" & ColCount + 1 & ":R26C" & ColCount + 1 & ""
    ActiveChart.SeriesCollection(3).Name = "=""Service Time"""
    With ActiveChart
        .HasTitle = True
        .ChartTitle.Characters.Text = "Customer Graph"
        .Axes(xlCategory, xlPrimary).HasTitle = True
        .Axes(xlCategory, xlPrimary).AxisTitle.Characters.Text = "Customer Number"
        .Axes(xlValue, xlPrimary).HasTitle = True
        .Axes(xlValue, xlPrimary).AxisTitle.Characters.Text = "Time"
    End With
    ActiveChart.SeriesCollection(1).Select
    With Selection.Border
        .Weight = xlThin
        .LineStyle = xlNone
    End With
    Selection.Shadow = False
    Selection.InvertIfNegative = False
    Selection.Interior.ColorIndex = xlNone
    ActiveChart.SeriesCollection(2).Select
    With Selection.Border
        .Weight = xlThin
        .LineStyle = xlAutomatic
    End With
    Selection.Shadow = False
    Selection.InvertIfNegative = False
    With Selection.Interior
        .ColorIndex = 3
        .Pattern = xlSolid
    End With
    With ActiveChart.ChartGroups(1)
        .Overlap = 100
        .GapWidth = 0
        .HasSeriesLines = False
    End With
    ActiveChart.SeriesCollection(3).Select
    With Selection.Border
        .Weight = xlThin
        .LineStyle = xlAutomatic
    End With
    Selection.Shadow = False
    Selection.InvertIfNegative = False
    With Selection.Interior
        .ColorIndex = 4
        .Pattern = xlSolid
    End With
    
    Sheet2.Calculate
    
    NewCustomerGraph = False
    
End Sub


Private Function GetColumnLetter(ColumnIndex As Integer) As String

    'This function returns a lettered Column Index from an integer value for a column
    
    'If ColumnIndex < 27 Then
    '    GetColumnLetter = Chr(64 + ColumnIndex)
    'ElseIf ColumnIndex Mod 26 = 0 Then
    '    GetColumnLetter = Chr(64 + Int((ColumnIndex + 1) / 27)) & Chr(65 + Int(ColumnIndex) Mod 27)
    'Else
    '    GetColumnLetter = Chr(64 + Int((ColumnIndex + 1) / 27)) & Chr(64 + ColumnIndex Mod 26)
    'End If
    
        'Latest updatese to the GetColumnLetter Function
    If ColumnIndex < 27 Then
        GetColumnLetter = Chr(64 + ColumnIndex)
    ElseIf ColumnIndex Mod 26 = 0 Then
        GetColumnLetter = Chr(64 + Int((ColumnIndex + 1) / 27)) & Chr(65 + Int(ColumnIndex) Mod 27)
    ElseIf ColumnIndex > 100 Then
        GetColumnLetter = Chr(64 + Int((ColumnIndex + 3) / 27)) & Chr(64 + ColumnIndex Mod 26)
    Else
        GetColumnLetter = Chr(64 + Int((ColumnIndex + 2) / 27)) & Chr(64 + ColumnIndex Mod 26)
    End If
    

End Function

' =========================================================================================

Rem Attribute VBA_ModuleType=VBAModule
Option VBASupport 1
Option Explicit

Public ReCalculation As Boolean 'used in the updating of the server graph
Public MaxCustomers As Integer  'used in the updating of the server graph
Public IgnoreCountRows As Boolean   'to avoid redrawing the new server graph
Public LastCalcOfCustomers As Integer   'remembers how many customers are entered in the system
Public NotNewGraph As Boolean   'distinguishes if a new ServerGraph has been selected to be created
    
Sub ServerGraph()
'
' ServerGraph Macro
' Macro recorded 4/10/2001 by Michael Thomas
'
    Dim ServerIdle As Boolean
    Dim i As Integer
    Dim ColCount As Integer 'Colstring As Integer, ', ServTimeCol As Integer
    Dim Servers As Integer
    Dim InputRange As Integer, StartRange As Integer, EndRange As Integer
    Dim CustomersSim As Integer, CustomersComp As Integer, MinCustomers As Integer
    
    
        'allow the worksheet to be written into
    Worksheets("Simulation").Unprotect
    Worksheets("Computations").Unprotect
    
        'counts how many customers are in the Simulation sheet
    i = 12
    Do Until Worksheets("Simulation").Cells(i, 1) = ""
        CustomersSim = CustomersSim + 1
        i = i + 1
    Loop
    
        'count how many customers are in the Computations sheet
    i = 12
    Do Until Worksheets("Computations").Cells(i, 1) = ""
        CustomersComp = CustomersComp + 1
        i = i + 1
    Loop
    
        'the minimum number of customer is the "Simulation" and "Computations" sheets is the
        'actual number of customers in the system
    MinCustomers = WorksheetFunction.Min(CustomersSim, CustomersComp)
        
        'checks if the number of customers has changed since the graph was last updated
        'if the number of customers has not changed, the graph is not altered (although the Time in the data sheet may have changed)
    If LastCalcOfCustomers = MinCustomers And NotNewGraph = True Then
        'do nothing
    Else
        ColCount = 0
        i = 1
            'used to count how many servers there are
        Do While Worksheets("Simulation").Cells(8, i) <> ""
            ColCount = ColCount + 1
            i = i + 1
        Loop
        
        Servers = (ColCount - 11) / 2
                'calculates how many columns are in 'Computations from the 'Simulation' sheet
        ColCount = ColCount - Servers + 3
        
            'Writes Label in Computations Sheet
        Worksheets("Computations").Cells(9, ColCount + 4) = "Time of Change in Server Activity"
        Worksheets("Computations").Cells(9, ColCount + 4).HorizontalAlignment = xlLeft
        
            'writes in changes to each servers status (ex. From busy to finished to idle etc)
            'these are the columns on the furthest right of the Computations spreadsheet
        For i = 1 To Servers
            'Tells the Formula where to get the values in the "Simulation" worksheet
            InputRange = 9 + ((i - 1) * 2)
            WriteFormulas (ColCount + 3 + i), InputRange, i, MinCustomers
            EndRange = ColCount + 3 + i
        Next i
        StartRange = ColCount + 3 + 1 '(Colcount + Distance from columns + First Server)
        
            'formats the above formula into time values (Values are changes in the server's activity)
        Worksheets("Computations").Select
        Worksheets("Computations").Range("" & GetColumnLetter(StartRange) & "12:" & GetColumnLetter(EndRange) & MinCustomers * 2 + 11).Select
        Selection.NumberFormat = "h:mm"
        Worksheets("Simulation").Select
        
        If Not ReCalculation = True Then
            'Creation of the Server Graph (added if the ServerGraph is called by the user, not by other code)
            Charts.Add
            ActiveChart.Location Where:=xlLocationAsObject, Name:="Simulation"
            ActiveChart.ChartType = xlBarClustered
        End If
                
                'below line resets the SourceData in the ServerGraph
            ActiveChart.SetSourceData Source:=Sheets("Computations").Range("R10"), PlotBy:=xlRows
            ActiveChart.SetSourceData Source:=Sheets("Computations").Range("" & GetColumnLetter(StartRange) & "11:" & GetColumnLetter(EndRange) & MinCustomers * 2 + 11), _
                PlotBy:=xlRows
         
         ServerIdle = True 'represents the change in the server's activity from idle to busy to idle etc
'______________________________________________________________________________________________
'Takes Exceedingly long during the recalculation
        For i = 1 To MinCustomers * 2
            ActiveChart.SeriesCollection(1).Select  'must be collection(1) because this is the bottom series
                                                    'RE: The bottom series is always moved up
            With Selection.Border
                .Weight = xlThin
                .LineStyle = xlAutomatic
            End With
            Selection.Shadow = False
            Selection.InvertIfNegative = False
            With Selection.Interior
                If ServerIdle = True Then
                    .ColorIndex = 4 'green
                    ServerIdle = False
                Else
                    .ColorIndex = 3 'red
                    ServerIdle = True
                End If
                .Pattern = xlSolid
            End With
        ActiveChart.ChartGroups(1).SeriesCollection(1).PlotOrder = (MinCustomers * 2 + 1) - i
        Next i
            
        CreateLegend MinCustomers
            
        ActiveChart.SeriesCollection(i - 1).Select
        With ActiveChart.ChartGroups(1)
            .Overlap = 100
            .GapWidth = 150
            .HasSeriesLines = False
        End With
        
    End If '(if number of customers hasn't changed)
'______________________________________________________________________________________________


        'this avoid a continuous loop between creating the graph and updating the graph
    IgnoreCountRows = True
    Sheet2.Calculate
    IgnoreCountRows = False
    
    ReCalculation = False
    NotNewGraph = False
    LastCalcOfCustomers = MinCustomers  'last recorded value of customers in the system
   
    End Sub

Private Function GetColumnLetter(ColumnIndex As Integer) As String

    'This function returns a lettered Column Index from an integer value for a column
    
    'If ColumnIndex < 27 Then
    '    GetColumnLetter = Chr(64 + ColumnIndex)
    'ElseIf ColumnIndex Mod 26 = 0 Then
    '    GetColumnLetter = Chr(64 + Int((ColumnIndex + 1) / 27)) & Chr(65 + Int(ColumnIndex) Mod 27)
    'Else
    '    GetColumnLetter = Chr(64 + Int((ColumnIndex + 1) / 27)) & Chr(64 + ColumnIndex Mod 26)
    'End If
    
       'Latest updates to the GetColumnLetter Function
    If ColumnIndex < 27 Then
        GetColumnLetter = Chr(64 + ColumnIndex)
    ElseIf ColumnIndex Mod 26 = 0 Then
        GetColumnLetter = Chr(64 + Int((ColumnIndex + 1) / 27)) & Chr(65 + Int(ColumnIndex) Mod 27)
    ElseIf ColumnIndex > 100 Then
        GetColumnLetter = Chr(64 + Int((ColumnIndex + 3) / 27)) & Chr(64 + ColumnIndex Mod 26)
    Else
        GetColumnLetter = Chr(64 + Int((ColumnIndex + 2) / 27)) & Chr(64 + ColumnIndex Mod 26)
    End If
    
    
End Function
Private Sub CreateLegend(Customers As Integer)

    Dim i As Integer
    
    With ActiveChart
        .HasLegend = True
        .Legend.Select
        Selection.Position = xlRight
        .SeriesCollection(Customers * 2 - 1).Name = "=""Busy"""
        .SeriesCollection(Customers * 2).Name = "=""Idle"""
        
        For i = 1 To Customers * 2 - 2
            .Legend.Select
            .Legend.LegendEntries(Customers * 2 - 1 - i).Select
            Selection.Delete
        Next i
        
        .HasDataTable = False
        .HasTitle = True
        .ChartTitle.Characters.Text = "Server Graph"
    End With
    
End Sub

Private Sub WriteFormulas(ColumnIndex As Integer, InputRange As Integer, ServerNumber As Integer, Customers As Integer)

    Dim i As Integer, j As Integer
    Dim Rangestart As String, RangeFinish As String

    Rangestart = GetColumnLetter(InputRange)
    RangeFinish = GetColumnLetter(InputRange + 1)

    j = 1
    Worksheets("Computations").Cells(11, ColumnIndex) = "Server " & ServerNumber & ""

    'This formula takes all of the times at which the server's activity changed and orders them _
    'in acending order.  It also inserts a dummy number(1) if the value happens to not be a number
    'The dummy number is needed so the graph can be created without crashing and without leaving _
    'out possible values that can be graphed
    For i = 12 To Customers * 2 + 11
        'Extends the server's Gantt chart bar to show idleness once there are no more customers(instead of showing nothing)
        'change the end of the "IF" statement to "1" instead of .000001 to show nothing
        Worksheets("Computations").Cells(i, ColumnIndex) = _
            "=IF(ISNUMBER(SMALL(Simulation!$" & Rangestart & "$12:Simulation!$" _
            & RangeFinish & "$" & Customers + 11 & "," & j & ")),SMALL(Simulation!$" & Rangestart & _
            "$12:Simulation!$" & RangeFinish & "$" & Customers + 11 & "," & j & "),1)"
        j = j + 1
    Next i

End Sub

'______________________________________________________________________________________________

Rem Attribute VBA_ModuleType=VBADocumentModule
Option VBASupport 1
Option Explicit

Public RowCountComp As Integer  'number of rows(customers) that was last counted in Computations
Public RowCountSim As Integer   ''number of rows that was last counted in Computations

    Private Sub Worksheet_Calculate()

    Dim N As Long, i As Integer
    Dim X As Double
    
    N = ActiveSheet.ChartObjects.Count
    ' Find open time in minutes
    X = ([close_time] - [start_time]) * 24 * 60
    ' Find how many 5 minute intervals in open time.
    ' Then divide by ten and take integer value.  The result,
    ' times 5 minutes, will be the interval length.  Therefore,
    ' there will be approximately ten intervals, and each interval
    ' will be a multiple of 5 minutes.
    X = Int(X / 10 / 5 + 1)
    For i = 1 To N
        With ActiveSheet.ChartObjects(i).Chart.Axes(xlValue)
            .MinimumScale = [start_time]
            .MaximumScale = [close_time]
            .MajorUnit = 1 / 24 / 60 * 5 * X
        End With
    Next i
        
        'if a new graph is drawn, the graph will not be updated
    If IgnoreCountRows = False Then
        CountRows
    End If
End Sub

       'Count the number of customers that will enter the system
Private Sub CountRows()
    
    Dim currentCountComp As Integer, currentCountSim As Integer, ColCount As Integer, EndRow As Integer
    Dim i As Integer, CellsThrown As Integer 'counts cells before customers are counted
    Dim CurrChart As Integer
    Dim UpdateCustomerGraph As Boolean
    'Dim SelectedCell As Variant
    
    currentCountSim = 0
    currentCountComp = 0
    
         'if there is no value for the 'RowCount' (first time sub is called)the _
        default value of 15 is used. This how many rows are initially in the sheets
    If RowCountSim = 0 Then
        RowCountSim = 15
    End If
    If RowCountComp = 0 Then
        RowCountComp = 15
    End If
    
    
    CellsThrown = 1
        'ignores the cells before customers are counted
    Do Until Worksheets("Computations").Cells(CellsThrown, 1) = "start"
        CellsThrown = CellsThrown + 1
    Loop
    
    i = CellsThrown + 1
        'counts number of customers entered into the 'Simulation' sheet
    Do Until Worksheets("Simulation").Cells(i, 1) = ""
        currentCountSim = currentCountSim + 1
        i = i + 1
    Loop
    
    i = CellsThrown + 1
        'counts number of customers entered into the 'Computations' sheet
    Do Until Worksheets("Computations").Cells(i, 1) = ""
        currentCountComp = currentCountComp + 1
        i = i + 1
    Loop
     
    EndRow = WorksheetFunction.Min(currentCountSim, currentCountComp) + 11
     
    ColCount = 0
    i = 1
    Do While Worksheets("Computations").Cells(8, i) <> ""
        ColCount = ColCount + 1
        i = i + 1
    Loop
    ColCount = ColCount - 2 'take away the columns that was added
    
    Worksheets("Simulation").Select
       
    If NewCustomerGraph = True Then
        UpdateCustomerGraph = True
    ElseIf currentCountComp <> RowCountComp Or currentCountSim <> RowCountSim Then
        UpdateCustomerGraph = True
    Else
        UpdateCustomerGraph = False
    End If
       
        'recalculates each chart in the sheet
    For CurrChart = 1 To ActiveSheet.ChartObjects.Count
    
            'updates the customer graph
        If ActiveSheet.ChartObjects(CurrChart).Chart.ChartType = xlBarStacked Then
                'will change dataseries in CustomerGraph if it is a new graph or _
                the number of customer has changed
            If UpdateCustomerGraph = True Then
                ActiveSheet.ChartObjects(CurrChart).Activate
                Worksheets("Computations").Select
                Worksheets("Simulation").Select
                ActiveChart.SeriesCollection(1).Values = "=Computations!R12C6:R" & EndRow & "C6"
                ActiveChart.SeriesCollection(2).Values = "=Computations!R12C" & ColCount + 2 & ":R" & EndRow & "C" & ColCount + 2 & ""
                ActiveChart.SeriesCollection(3).Values = "=Computations!R12C" & ColCount + 1 & ":R" & EndRow & "C" & ColCount + 1 & ""
            End If
        Else
                'only updates ServerGraph if there has been changes to the number of customers
            If currentCountComp <> RowCountComp Or currentCountSim <> RowCountSim Then
                ActiveSheet.ChartObjects(CurrChart).Activate
                UpdateServerGraph
            End If
        End If
    
    Next CurrChart
    
    RowCountComp = currentCountComp
    RowCountSim = currentCountSim

End Sub

Private Sub UpdateServerGraph()
    
    ReCalculation = True
    NotNewGraph = True
    Module2.ServerGraph
   
End Sub


