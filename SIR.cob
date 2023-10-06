              >>SOURCE FORMAT FREE
       *>  GROUP 4 SHOE INTERFACE AND RECEIPT SYSTEM
       *>  MEMBERS:
       *>  BERJA, ROBIN ART G.
       *>  FAINA, ANGELINE G.
       *>  FELIX, RICHARD JAN ANGEL A.
       *>  SAN ANTONIO, CHARMAINE LOUIS A.
       *>  VILLACORTE, LOVELY O.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIR-SYSTEM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       *>  THE FIRST 3 FILES ARE THE MAIN FILES FOR THE SYSTEM
       *>  THESE FILES ARE INDEXED FOR THE PURPOSE OF STORING A RECORD
       *>  WITH A RECORD KEY WITH THEM
       *>  STOCKFILE IS FOR STORING THE STOCKS ACCORDING TO IT'S ITEM NAME, COLOR AND SIZE
           SELECT StockFile ASSIGN TO "Stock.txt"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS StockName.
       *>  INVENTORY FILE IS FOR STORING ALL AVAILABLE COLORS AND SIZE
       *>  IT ALSO STORES THE OVERALL STOCKS OF AN ITEM
           SELECT InventoryFile ASSIGN TO "Inventory.txt"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ItemName.
       *>  BRANDFILE IS FOR STORING THE SOLD ITEMS AFTER THE PURCHASE
       *>  IT STORES THE OVERALL SELLS OF A BRAND
           SELECT BrandFile ASSIGN TO "TopBrand.txt"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BrandName.
       *>  THESE REMAINING FILE ARE FOR THE LOGICAL TRANSFER
       *>  AND USE OF DATA FOR FILE HANDLING
           SELECT ReceiptFile ASSIGN TO "Receipt.txt"
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL.

           SELECT TempFile ASSIGN TO "Temp.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

           SELECT TempFile2 ASSIGN TO "Temp2.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

           SELECT WorkFile ASSIGN TO "Work.tmp".
           SELECT WorkFile2 ASSIGN TO "Work2.tmp".
           SELECT TopBrandFile ASSIGN TO "Brand.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CheapFile ASSIGN TO "Cheap.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ExpensiveFile ASSIGN TO "Expensive.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       *>  HERE ARE ALL THE FILE DESCRIPTIONS FOR ALL THE FILE USED IN THE SYSTEM
       FD StockFile.
           01 StockData.
               02 StockName.
                   03 STItmName PIC X(20).
                   03 ColorCode PIC X(3).
                   03 SizeCode PIC X(2).
               02 PerStock PIC ZZZZZ9.
       FD InventoryFile.
           01 ItemData.
               02 ItemName.
                   03 Brand PIC X(3).
                   03 ItmName PIC X(17).
               02 AvlColor PIC X(20).
               02 AvlSizes PIC X(15).
               02 Price PIC ZZZZ9.99.
               02 Stock PIC ZZZZZ9.
       FD BrandFile.
           01 BrandData.
               02 BrandName PIC X(20).
               02 Sold PIC ZZ9.
       FD ReceiptFile.
           01 RcptInfo.
               02 ITEM-DESC.
                   03 SHOE-NAME.
                       04 RBrand PIC X(3).
                       04 RItmName PIC X(17).
                   03 SHOE-COLOR PIC X(15).
                   03 SHOE-SIZE PIC X(2).
                   03 ITEM-QTY PIC ZZ9.
                   03 ITEM-PRICE PIC ZZZZZ9.99.
                   03 ITEM-AMOUNT PIC ZZZZZ9.99.
       FD TopBrandFile.
           01 TBBrandData.
               02 TBBrandName PIC X(20).
               02 TBSold PIC ZZ9.
       FD TempFile2.
           01 TFBrandData.
               02 TFBrandName PIC X(20).
               02 TFSold PIC ZZ9.
       SD WorkFile2.
           01 WFBrandData.
               02 WFBrandName PIC X(20).
               02 WFSold PIC ZZ9.
       FD TempFile.
           01 TempItem.
               02 TItemName PIC X(20).
               02 TPrice PIC ZZZZ9.99.
       SD WorkFile.
           01 WTempItem.
               02 WTItemName PIC X(20).
               02 WTPrice PIC ZZZZ9.99.
       FD CheapFile.
           01 CTempItem.
               02 CTItemName PIC X(20).
               02 CTPrice PIC ZZZZ9.99.
       FD ExpensiveFile.
           01 ETempItem.
               02 EItemName PIC X(20).
               02 EPrice PIC ZZZZ9.99.

       WORKING-STORAGE SECTION.
       *>  IN THIS SECTION ARE ALL THE FIELD AND RECORDS USED FOR THE SYSTEM
       *>  SOME OF IT ARE FOR TRANFER OF DATA FROM FILES
       *>  SOME ARE FOR COMPUTATION
       *>  AND THE OTHERS ARE FOR BETTER DISPLAY
       01 Num      PIC 9.
       01 OpMen    PIC X VALUE 'Y'.
       01 WSEOF   PIC A(1).

       01 Choice PIC 9.
       01 StayOpen PIC X VALUE 'Y'.
       01 ItemExist Pic X.
       01 WSItemData.
               02 WSItemName.
                   03 WSBrand PIC X(3).
                   03 WSItmName PIC X(17).
               02 WSAvlColor PIC X(20).
               02 WSAvlSizes PIC X(15).
               02 WSPrice PIC ZZZZ9.99.
               02 WSStock PIC ZZZZZ9.

       01 WSCrntdate PIC X(10).
       01 WSCustName PIC X(20).
       01 WSRcptInfo.
               02 WSITEM-DESC.
                   03 WSSHOE-NAME.
                       04 WSRBrand PIC X(3).
                       04 WSRItmName PIC X(17).
                   03 WSSHOE-COLOR PIC X(15).
                   03 WSSHOE-SIZE PIC ZZZZ9.
                   03 WSITEM-QTY PIC ZZZZZ9.
                   03 WSITEM-PRICE PIC ZZZZZ9.99.
                   03 WSITEM-AMOUNT PIC ZZZZZ9.99.

       01 WSBrandData.
               02 WSBrandName PIC X(20).
               02 WSSold PIC ZZ9.

       01 WSTOTAL-BILL PIC ZZZZZ9.99.
       01 WSChange PIC ZZZZZ9.99.
       01 AmountReceived PIC 99999999.
       01 WSTemp PIC 9 VALUE 1.
       01 TempPrice PIC 999999.
       01 TempAmount PIC 999999.
       01 TempQty PIC 99.
       01 TempTotal PIC 999999.
       01 TempChange PIC 999999.
       01 PrintTotal PIC ZZZZZ9.99.
       01 PrintChange PIC ZZZZZ9.99.
       01 PrintAmount PIC ZZZZZ9.99.
       01 Cash PIC ZZZZZ9.99.
       01 TempStock PIC 999999.
       01 TempStock2 PIC 999999.
       01 ReStock PIC 999999.
       01 TempSold PIC 999.
       01 TempColor PIC X(10).
       01 Color1 PIC X(10).
       01 Color2 PIC X(10).
       01 Color3 PIC X(10).
       01 IDColor1 PIC X(20).
       01 IDColor2 PIC X(20).
       01 WSColor1 PIC X(3).
       01 WSColor2 PIC X(3).
       01 TempSize PIC X(2).
       01 Size1 PIC X(2).
       01 Size2 PIC X(2).
       01 Size3 PIC X(2).
       01 Size4 PIC X(2).
       01 Size5 PIC X(2).
       01 IDSize1 PIC X(15).
       01 IDSize2 PIC X(15).
       01 IDSize3 PIC X(15).
       01 IDSize4 PIC X(15).
       01 IDSize5 PIC X(15).
       01 ColorCode2 PIC X(3).
       01 SizeCode2 PIC X(2).
       01 TempC PIC X(3).
       01 TempS PIC X(2).
       01 FinalColor PIC X(20).
       01 Clear PIC X(20).
       01 FinalSIze PIC X(15).
       01 SortItem.
               02 SItemName PIC X(20).
               02 SPrice PIC ZZZZ9.99.

       PROCEDURE DIVISION.
       *>  A SHORT DESCRIPTION OF OUR SYSTEM
       DESCRIPTION.
            DISPLAY  "SIR SYSTEM: SHOE INTERFACE & RECEIPT SYSTEM".
            DISPLAY  "System for Item and Management Brand Tracking".
       *>  MAIN PARAGRAPH WHEREIN IT SHOWS THE MAIN FEATURES OF THE SYSTEM
       *>  GIVING THE OPTIONS FOR THE MAIN MENU
       MAIN.
           PERFORM UNTIL OpMen='N'
               DISPLAY SPACE
               DISPLAY "MAIN MENU:"
               DISPLAY "0.EXIT"
               DISPLAY "1.RECEIPT"
               DISPLAY "2.INVENTORY"
               DISPLAY "3.ITEM RECOMMENDATION"
               ACCEPT Num
               EVALUATE Num
                   WHEN 1 PERFORM Receipt
                   WHEN 2 PERFORM INVENTORY
                   WHEN 3 PERFORM ItemRecommend
                   WHEN OTHER MOVE 'N' TO OpMen
               END-EVALUATE
           END-PERFORM.

       STOP RUN.
       *>  START OF GETTING THE DATA FOR THE RECEIPT
       *>  INCLUDES THE CREATION OF RECEIPT FILE
       *>  THEN IT WILL PROCEED TO GET THE ITEMS
       Receipt.
           DISPLAY "WELCOME TO RECEIPT".
           DISPLAY SPACE
           OPEN OUTPUT ReceiptFile.
           CLOSE ReceiptFile.
           DISPLAY "Enter Date Today (MM/DD/YYYY): " .
           ACCEPT WSCrntdate.
           DISPLAY "Enter Customer Name: "
           ACCEPT WSCustName.
           GO TO RcptItems.
       *>  AN OPTION FOR A NEW RECEIPT OR GOING BACK TO MAIN MENU
       RcptOptions.
           DISPLAY "1. Go Back To Main Menu | 2. New Receipt".
           ACCEPT Choice.
           EVALUATE CHOICE
               WHEN 1 PERFORM MAIN
               WHEN 2 PERFORM Receipt
               WHEN OTHER STOP RUN
           END-EVALUATE.
       *>  THIS PARAGRAPH IS WHERE IT GETS THE ITEMS
       *>  MANAGES EVERY ITEM FROM FILE TO FILE
       RcptItems.
       *>  RECEIPT FILE IS EXTEND BECAUSE THIS PARAGRAPH LOOPS
       *>  AND AFTER IT LOOPS WE WANT TO APPEND THE NEXT ITEMS
       *>  INDEXED FILES ARE I-O FOR THE PURPOSE OF READ,WRITE,DELETE, AND REWRITE RECORDS
           OPEN EXTEND ReceiptFile.
           OPEN I-O InventoryFile.
           OPEN I-O BrandFile.
           OPEN I-O StockFile.
       *>  GETTING THE ITEM AND SCANS THE INVENTORY FILE IF IT IS AVAILABLE
           DISPLAY "Enter Bought Items:"
           ACCEPT SHOE-NAME
           MOVE SHOE-NAME TO ItemName
           MOVE SHOE-NAME TO STItmName
           MOVE 'Y' TO ItemExist.
           READ InventoryFile
               INVALID KEY MOVE 'N' TO ItemExist
           END-READ
       *>  IF IT DOESN'T EXIST IT WILL GO BACK TO GET A NEW ITEM
           IF ItemExist='N'
               DISPLAY "Item Does not Exist"
               CLOSE ReceiptFile
               CLOSE InventoryFile
               CLOSE BrandFile
               CLOSE StockFile
               GO TO RcptItems
       *>  IF IT EXIST IT WILL DISPLAY THE PRICE STORED FROM THE INVENTORY
           ELSE
               DISPLAY "Item : " ItemName
               DISPLAY "Price : " Price
               MOVE PRICE TO ITEM-PRICE
               MOVE PRICE TO TempPrice
               MOVE Stock TO TempStock
           END-IF.
       *>  CONTINUES TO GET THE DETAILS OF THE ITEM
           DISPLAY "Enter Item Color: "
           ACCEPT SHOE-COLOR.
           MOVE SHOE-COLOR TO ColorCode.
           DISPLAY "Enter Item Size: "
           ACCEPT SHOE-SIZE.
           MOVE SHOE-SIZE TO SizeCode.
           DISPLAY "Enter Item Quantity: "
           ACCEPT ITEM-QTY.
           MOVE ITEM-QTY TO WSITEM-QTY.
           MOVE ITEM-QTY TO TempQty.
           MOVE 'Y' TO ItemExist.
       *>  AFTER GETTING THE COLOR AND SIZE IT SCANS STOCKFILE IF IT IS AVAILABLE
           READ StockFile
               INVALID KEY MOVE 'N' TO ItemExist
           END-READ
           IF ItemExist='N'
               DISPLAY StockName
               DISPLAY "Specified Item is not Available"
               CLOSE ReceiptFile
               CLOSE InventoryFile
               CLOSE BrandFile
               CLOSE StockFile
               GO TO RcptItems
       *>  IF IT IS AVAILABLE IT WILL SUBTRACT THE PURCHASED QUANTITY FROM THE STOCK
       *>  AFTER THAT IT WILL UPDATE THE STOCKFILE
           ELSE
               MOVE PerStock TO TempStock2
               SUBTRACT TempQty FROM TempStock2 GIVING ReStock
               MOVE ReStock TO PerStock
           END-IF.
           IF ReStock IS GREATER THAN 0
               REWRITE StockData
               END-REWRITE
           CLOSE StockFile
           END-IF.
       *>  IF THE LAST ITEM WAS SOLD IT WILL REMOVE THAT ITEM FROM THE STOCKFILE
           IF ReStock IS LESS THAN 1
               DELETE StockFile
                   NOT INVALID KEY DISPLAY "Last Stock for this Item Color and Size Sold."
               END-DELETE
               DISPLAY 'REMOVING COLOR OR SIZE IN THE LIST'
               DISPLAY SPACE
       *>  GETTING ALL THE AVAILABLE COLORS AND SIZE FROM THE INVENTORY
               UNSTRING AvlColor DELIMITED BY ','
               INTO Color1 Color2

               UNSTRING AvlSizes DELIMITED BY ','
               INTO Size1 Size2 Size3 Size4 Size5

           STRING Size2 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size3 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size4 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size5 DELIMITED BY SPACE
               INTO IDSize1

           STRING Size1 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size3 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size4 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size5 DELIMITED BY SPACE
               INTO IDSize2

           STRING Size2 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size1 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size4 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size5 DELIMITED BY SPACE
               INTO IDSize3

           STRING Size2 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size3 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size1 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size5 DELIMITED BY SPACE
               INTO IDSize4

           STRING Size2 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size3 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size4 DELIMITED BY SPACE
                  ',' DELIMITED BY SIZE
                  Size1 DELIMITED BY SPACE
               INTO IDSize5
           MOVE SizeCode TO SizeCode2
           MOVE ColorCode TO ColorCode2
           CLOSE StockFile
           MOVE 'N' TO ItemExist
           MOVE'N' TO WSEOF
       *>  READS THE STOCKFILE IF THAT COLOR FOR AN ITEM IS STILL AVAILABLE
           OPEN I-O StockFile
           PERFORM UNTIL WSEOF='Y'
               READ StockFile NEXT RECORD INTO StockData
                   AT END MOVE 'Y' TO WSEOF
                   NOT AT END MOVE ColorCode TO TempC
                   IF ColorCode2 IS EQUAL TO TempC AND SHOE-NAME IS EQUAL TO STItmName
                       MOVE 'Y' TO ItemExist
                       MOVE 'Y' TO WSEOF
                   END-IF
               END-READ
           END-PERFORM
           CLOSE StockFile

           MOVE Color1 TO WSColor1
           MOVE Color2 TO WSColor2
       *>  IF IT IS NOT AVAILABLE ANYMORE IT WILL BE REMOVED FROM THE INVENTORY
           IF ItemExist = "N"
               EVALUATE ColorCode2
               WHEN WSColor1 MOVE Color2 TO AvlColor
               WHEN WSColor2 MOVE Color1 TO AvlColor
               END-EVALUATE
               REWRITE ItemData
               END-REWRITE
           END-IF
       *>  READS THE STOCKFILE IF THE SIZE IS STILL AVAILABLE
           OPEN I-O StockFile
           MOVE 'N' TO ItemExist
           MOVE'N' TO WSEOF
           PERFORM UNTIL WSEOF='Y'
               READ StockFile NEXT RECORD INTO StockData
                   AT END MOVE 'Y' TO WSEOF
                   NOT AT END MOVE SizeCode TO TempS
                   IF SizeCode2 IS EQUAL TO TempS AND SHOE-NAME IS EQUAL TO STItmName
                       MOVE 'Y' TO ItemExist
                       MOVE 'Y' TO WSEOF
                   END-IF
               END-READ
           END-PERFORM
           CLOSE StockFile.
       *>  IF IT IS NOT IN THE STOCK FILE IT WILL STRING THE REMAINING
       *>  SIZE WITHOUT THE SIZE REMOVED
           IF ItemExist = 'N'
               EVALUATE SizeCode2
               WHEN Size1 MOVE IDSize1 TO AvlSizes
               WHEN Size2 MOVE IDSize2 TO AvlSizes
               WHEN Size3 MOVE IDSize3 TO AvlSizes
               WHEN Size4 MOVE IDSize4 TO AvlSizes
               WHEN Size5 MOVE IDSize5 TO AvlSizes
               END-EVALUATE
               REWRITE ItemData
               END-REWRITE
           END-IF.
       *>  THIS PART EVALUTES THE FIRST 3 LETTERS OF THE ITEM NAME
       *>  WHICH GIVES OFF THE BRAND NAME
           EVALUATE Brand
           WHEN 'ADS' MOVE 'ADIDAS' TO BrandName
           WHEN 'PUM' MOVE 'PUMA' TO BrandName
           WHEN 'SKE' MOVE 'SKETCHERS' TO BrandName
           WHEN 'FIL' MOVE 'FILA' TO BrandName
           WHEN 'NIK' MOVE 'NIKE' TO BrandName
           END-EVALUATE.
       *>  IT WILL THEN READ AND SEARCH FOR THE BRAND IN THE BRANDFILE
           MOVE 'Y' TO ItemExist.
           READ BrandFile
                   INVALID KEY MOVE 'N' TO ItemExist
           END-READ.
       *>  IF THE BRAND IS ALREADY IN THE FILE IT WILL JUST INCREASE THE
       *>  AMOUNT OF SOLD ITEMS FOR THAT BRAND BYT THE QUANTITY SOLD IN
       *>  THE RECEIPT
           IF ItemExist='Y'
               MOVE Sold TO TempSold
               ADD TempQty TO TempSold GIVING Sold
               REWRITE BrandData
               END-REWRITE.
       *>  IF THE BRAND IS NOT YET IN THE FILE IT WILL JUST WRITE THE BRAND
       *>  WITH THE SOLD ITEMS BEING THE QUANTITY SOLD IN THE RECEIPT
           IF ItemExist='N'
               MOVE TempQty TO Sold
               WRITE BrandData
               END-WRITE
           END-IF.
           SUBTRACT TempQty FROM TempStock GIVING ReStock.
       *>  JUST LIKE THE STOCKFILE BEFORE WHEN THE ITEM IN THE INVENTORY
       *>  HAVE NO MORE STOCKS LEFT THEN THAT ITEM WILL BE DELETED
           IF ReStock IS EQUAL TO 0
               DELETE InventoryFile
                   NOT INVALID KEY DISPLAY "Last Stock for this Item Sold."
               END-DELETE.
           IF ReStock IS LESS THAN 1
               DELETE InventoryFile
                   INVALID KEY DISPLAY "OUT OF STOCK"
               END-DELETE
       *>  IF THAT ITEM STILL HAS REMAINING STOCKS AFTER THE PURCHASE
       *>  THEN THE INVENTORY WILL JUST UPDATE THE STOCK
           ELSE
               MOVE ReStock TO Stock
               REWRITE ItemData
               END-REWRITE
           END-IF.
       *>  THE COMPUTATION FOR THE TOTAL PRICE STARTS HERE
           COMPUTE TempAmount = TempPrice * TempQty.
           MOVE TempAmount TO ITEM-AMOUNT.
           MOVE TempAmount TO PrintAmount.
           DISPLAY "Total: " PrintAmount.
           COMPUTE TempTotal = TempTotal + TempAmount.
           MOVE TempTotal TO PrintTotal
           Display "Total Price: " PrintTotal.
           WRITE RcptInfo.
           OPEN I-O StockFile.
           CLOSE StockFile.
           CLOSE BrandFile.
           CLOSE InventoryFile.
           CLOSE ReceiptFile.
           DISPLAY SPACE
           DISPLAY "Continue Press 1 | Print Receipt 0"
           ACCEPT WSTemp.
       *>  WE USE AN EVALUATE LOOP HERE FOR USERS TO HAVE MORE OPTIONS
           EVALUATE WSTemp
               WHEN 0 GO TO PrintReceipt
               WHEN 1 GO TO RcptItems
           END-EVALUATE.
       *>  THIS PARAGRAGH IS RESPONSIBLE FOR THE DISPLAY OF THE RECIPT
       *>  AND ALSO THE COMPUTAION FOR THE CHANGE
       PrintReceipt.
           DISPLAY SPACE
           DISPLAY "Amount Received: "
           ACCEPT AmountReceived.
       *>  IT WILL IDENTIFY HERE IF THE AMOUNT RECEIVED IS ENOUGH
       *>  IF THE AMOUNT RECEIVED IS NOT ENOUGH IT WILL THEN
       *>  GO BACK TO GET A NEW AMOUNT UNTIL IT IS ENOUGH
       *>  OFCOURSE THIS IS TO AVOID LOSING MONEY OF THE STORE
           IF AmountReceived IS LESS THAN TempTotal
               DISPLAY "Received Cash is not enough"
               GO TO PrintReceipt
           END-IF.
           MOVE AmountReceived TO Cash.
           SUBTRACT TempTotal FROM AmountReceived GIVING TempChange.
           MOVE TempChange TO PrintChange.
           MOVE 0 TO TempTotal.
           DISPLAY SPACE
           DISPLAY "******************************************************************".
           DISPLAY "                        SHOENIVERSE COMPANY                       ".
           DISPLAY "                1016 Anonas, Sta. Mesa, Manila                    ".
           DISPLAY "******************************************************************".
           DISPLAY "DATE: " WSCrntdate
           DISPLAY "CUSTOMER NAME: " WSCustName
           DISPLAY SPACE
           DISPLAY "ITEMS               COLOR          SZ  QTY         PRICE"
           OPEN INPUT ReceiptFile.
           MOVE "N" TO WSEOF.
           PERFORM UNTIL WSEOF='Y'
               READ ReceiptFile INTO WSRcptInfo
                   AT END MOVE 'Y' TO WSEOF
                   NOT AT END DISPLAY WSRcptInfo
               END-READ
           END-PERFORM.
           CLOSE ReceiptFile.
           DISPLAY SPACE
           DISPLAY"                                         TOTAL PRICE:"PrintTotal.
           DISPLAY"                                     AMOUNT RECEIVED:"Cash.
           DISPLAY"                                              CHANGE:"PrintChange.
       *>  AFTER DISPLAYING THE RECIPT IT WILL GO BACK TO ASK
       *>  IF THE USER WANTS A NEW RECEIPT OR TO GO TO MAIN MENU
           GO TO RcptOptions.
       *>  THE 2ND OPTION IN THE MAIN MENU
       *>  THIS PARAGRAGH CONSIST OF ALL THE ACTIONS FOR THE INVENTORY
       *>  ALSO THE DISPLAY OF ALL THE ITEMS
       INVENTORY.
           DISPLAY "WELCOME TO INVENTORY".
           DISPLAY SPACE
           OPEN I-O StockFile.
           OPEN I-O InventoryFile.
           DISPLAY "INVENTORY LIST"
           DISPLAY SPACE
       *>  DISPLAY OF ALL ITEMS IN THE INVENTORY ARRANGED IN A TABLE
           DISPLAY "ITEMS               COLORS              SIZES           PRICE      STOCK"
           MOVE "N" TO WSEOF.
           PERFORM UNTIL WSEOF='Y'
               READ InventoryFile NEXT RECORD INTO WSItemData
                   AT END MOVE 'Y' TO WSEOF
                   NOT AT END DISPLAY WSItemData
               END-READ
           END-PERFORM.
               MOVE 'Y' TO StayOpen.
       *>  THE OPTIONS YOU CAN DO IN THE INVENTORY
               PERFORM UNTIL StayOpen='N'
                   DISPLAY SPACE
                   DISPLAY "Inventory Settings"
                   DISPLAY "1: Add New Item"
                   DISPLAY "2: Add New Color or Size"
                   DISPLAY "3: Add Stock"
                   DISPLAY "4: Search Item"
                   DISPLAY "5: Advance Inventory Settings"
                   DISPLAY "0: EXIT"
                   ACCEPT Choice
                   EVALUATE Choice
                       WHEN 1 PERFORM AddItem
                       WHEN 2 PERFORM AddExist
                       WHEN 3 PERFORM AddStock
                       WHEN 4 PERFORM GetItem
                       WHEN 5 PERFORM AIS
                       WHEN OTHER MOVE 'N' TO StayOpen
                    END-EVALUATE
               END-PERFORM.
               CLOSE StockFile.
               CLOSE InventoryFile.
               EXIT.
       *>  THE ADVANCE INVENTORY SETTINGS PARAGRAPH
       *>  THIS PARAGRAPH IS FOR THE ADVANCE ACTIONS A USER CAN DO
       *>  IN THE INVENTORY BECAUSE THE INVENTORRY SETTINGS
       *>  CONSIST OF THE ADDING OF ITEMS WITHOUT ANY DELETION OR CHANGING AN ITEM
       AIS.
           PERFORM UNTIL StayOpen='N'
               DISPLAY SPACE
               DISPLAY "ADVANCE INVENTORY SETTINGS"
               DISPLAY "1. Display Stocks"
               DISPLAY "2. Change Item Price"
               DISPLAY "ENTER ANY KEY TO EXIT"
               ACCEPT Num
               EVALUATE Num
                   WHEN 1 PERFORM DisplayStock
                   WHEN 2 PERFORM EditPrice
                   WHEN OTHER MOVE 'N' TO StayOpen
               END-EVALUATE
           END-PERFORM.
       *>  THIS PARAGRAPH DISPLAYS THE STOCK OF ITEMS
       *>  EACH SIMILAR ITEM WITH DIFFERENT COLOR OR SIZE HAVE THEIR OWN
       *>  STOCK STORED IN HERE AND DISPLAYS IT
       DisplayStock.
           DISPLAY SPACE
           DISPLAY "ITEM CODE                    STOCK"
           MOVE 'N' TO WSEOF
           PERFORM UNTIL WSEOF='Y'
               READ StockFile NEXT RECORD INTO StockData
                   AT END MOVE 'Y' TO WSEOF
                   NOT AT END DISPLAY StockData
               END-READ
           END-PERFORM.
       *>  THIS IS TO CHANGE THE PRICE OF AN ITEM
       *>  THIS IS NOT MENTIIONED IN THE PRACTICE DEFENSE
       *>  BUT WE THINK IT IS ALSO NEEDED WHEN THERE IS A SALE IN THE STORE
       *>  THAT WHY IT IS INSIDE THE ADVANCE INVENTORY SETTINGS
       EditPrice.
           DISPLAY SPACE
           DISPLAY "Edit Item Price"
           DISPLAY SPACE
           DISPLAY "Item:"
           ACCEPT ItemName.
           READ InventoryFile
                   INVALID KEY MOVE 'N' TO ItemExist
               END-READ
               IF ItemExist='N'
                   DISPLAY "Item Does not Exist"
               ELSE
                   DISPLAY "New Price:"
                   ACCEPT Price
                   DISPLAY "Price Updated Successfully"
               END-IF.
           REWRITE ItemData
           END-REWRITE.
       *>  THE PARAGRAPH FOR ADDING NEW ITEM
       *>  IT WILL CREATE A NEW RECORD FOR BOTH THE INVENTORY AND STOCK FILE
       AddItem.
               DISPLAY "Enter Item Name : "
               ACCEPT ItemName.
               MOVE ItemName TO STItmName.
               DISPLAY "Enter Color : "
               ACCEPT AvlColor.
               MOVE AvlColor TO ColorCode.
               DISPLAY "Enter Size : "
               ACCEPT AvlSizes.
               MOVE AvlSizes TO SizeCode

               DISPLAY "Enter Price : "
               ACCEPT Price.
               DISPLAY "Stock : "
               ACCEPT Stock.
               MOVE Stock TO PerStock.
               WRITE StockData
                   INVALID KEY DISPLAY "Item Already Exist"
               END-WRITE.
               WRITE ItemData
                   NOT INVALID KEY DISPLAY "Item Succesfully Added."
               END-WRITE.
       *>  THIS PARAGRAPGH IS FOR ADDING NEW COLOR OR SIZE FOR AN EXISTING ITEM
       AddExist.
               MOVE 'Y' TO ItemExist.
               DISPLAY SPACE
               DISPLAY "Enter Item to Find : "
               ACCEPT ItemName.
               READ InventoryFile
                   INVALID KEY MOVE 'N' TO ItemExist
               END-READ
               IF ItemExist='N'
                   DISPLAY "Item Does not Exist"
               ELSE
                   DISPLAY "Item Found"
                   MOVE ItemName TO STItmName
                   DISPLAY SPACE
                   DISPLAY " Add New Size(1) | Add New Color and Size(2)"
                   ACCEPT Choice
                   EVALUATE Choice
                       WHEN 1 PERFORM AddSize
                       WHEN 2 PERFORM AddBoth
                   END-EVALUATE
               END-IF.
       *>  ADDING NEW SIZE FOR AN EXISTING ITEM
       *>  THE NEW ITM WILL BE ADDED AS A STRING IN THE INVENTORY FILE
       AddSize.
           DISPLAY SPACE
           DISPLAY "Enter New Size :"
           ACCEPT TempSize
           MOVE TempSize TO SizeCode
           MOVE Clear TO FinalSIze
           STRING AvlSizes DELIMITED BY SPACE
           ',' DELIMITED BY SIZE
           TempSize DELIMITED BY SPACE
           INTO FinalSize.
           MOVE FinalSIze TO AvlSizes
           DISPLAY "Enter Color Code:"
           ACCEPT ColorCode
           DISPLAY "Enter Stock"
           ACCEPT TempStock
           MOVE TempStock TO PerStock
           MOVE Stock TO TempStock2
           COMPUTE ReStock= TempStock + TempStock2
           MOVE ReStock TO Stock
           REWRITE ItemData
                   INVALID KEY DISPLAY "Item not Updated"
                   NOT INVALID KEY DISPLAY "Item Succesfully Updated."
           END-REWRITE.
           WRITE StockData
           END-WRITE.
       *>  ADDING NEW COLOR AND SIZE AT THE SAME TIME IN THE INVENTORY
       *>  IT WILL ALSO CREATE A RECORD OF ITS OWN STOCK INSIDE STOCKFILE
       AddBoth.
           DISPLAY SPACE
           DISPLAY "Enter New Color :"
           ACCEPT TempColor
           MOVE Clear TO FinalColor
           MOVE TempColor TO ColorCode
           STRING AvlColor DELIMITED BY SPACE
           ',' DELIMITED BY SIZE
           TempColor DELIMITED BY SPACE
           INTO FinalColor.
           MOVE FinalColor TO AvlColor
           DISPLAY "Enter New Size :"
           ACCEPT TempSize
           MOVE Clear TO FinalSIze
           MOVE TempSize TO SizeCode
           STRING AvlSizes DELIMITED BY SPACE
           ',' DELIMITED BY SIZE
           TempSize DELIMITED BY SPACE
           INTO FinalSize.
           MOVE FinalSIze TO AvlSizes
           DISPLAY "Enter Stock"
           ACCEPT TempStock
           MOVE TempStock TO PerStock
           MOVE Stock TO TempStock2
           COMPUTE ReStock= TempStock + TempStock2
           MOVE ReStock TO Stock
           REWRITE ItemData
                   INVALID KEY DISPLAY "Item not Updated"
                   NOT INVALID KEY DISPLAY "Item Succesfully Updated."
           END-REWRITE.
           WRITE StockData
           END-WRITE.
       *>  THIS PARAGRAPGH IS FOR THE PURPOSE OF RESTOCKING AND ADDING STOCKS TO AN ITEM
       *>  IT ONLY ADDS AND AS INSTRUCTED THERE ARE NO DELETE OR CHANGE JUST ADD
       AddStock.
              DISPLAY SPACE
              DISPLAY "Enter Item Name :"
              ACCEPT ItemName
              MOVE ItemName TO STItmName
              DISPLAY "Enter Color Code :"
              ACCEPT ColorCode
              DISPLAY "Enter Size Code :"
              ACCEPT SizeCode
              MOVE 'Y' TO ItemExist.
              READ StockFile
                   INVALID KEY MOVE 'N' TO ItemExist
               END-READ
               IF ItemExist='N'
                   DISPLAY "Item Does not Exist"
               ELSE
                   DISPLAY "Enter New Stock to be added: "
                   ACCEPT TempStock
                   MOVE PerStock TO TempStock2
                   COMPUTE ReStock = TempStock + TempStock2
                   MOVE ReStock TO PerStock
                   READ InventoryFile
                   MOVE Stock TO TempStock2
                   COMPUTE ReStock = TempStock + TempStock2
                   MOVE ReStock TO Stock
               END-IF.
           REWRITE ItemData
               INVALID KEY DISPLAY "Item not Updated"
               NOT INVALID KEY DISPLAY "Item Succesfully Updated."
           END-REWRITE.
           REWRITE StockData
           END-REWRITE.
       *>  THIS PARAGRAPH IS FOR SEARCHING AN ITEM IN THE INVENTORY
       *>  IT WILL DISPLAY ALL THE AVAILABLE COLORS AND SIZES OF THAT ITEM
       *>  AND ALSO ITS PRICE
       GetItem.
               MOVE 'Y' TO ItemExist.
               DISPLAY SPACE
               DISPLAY "Enter Item to Find : "
               ACCEPT ItemName.
               READ InventoryFile
                   INVALID KEY MOVE 'N' TO ItemExist
               END-READ
               IF ItemExist='N'
                   DISPLAY "Item Does not Exist"
               ELSE
                   DISPLAY "Item : " ItemName
                   DISPLAY "Available Colors : " AvlColor
                   DISPLAY "Available Sizes: " AvlSizes
                   DISPLAY "Price : " Price
                   DISPLAY "Stock : " Stock
               END-IF.
       *>  THE 3RD OPTION IN THE MAIN MENU
       *>  THIS IS WHERE THE SOLD ITEMS ARE DISPLAYED AS THE BRAND
       *>  ALL ITEMS HERE ARE SORTED
       ItemRecommend.
           DISPLAY SPACE
           DISPLAY "Item Recommendation"
           DISPLAY "1. Top Brand Sold"
           DISPLAY "2. Cheapest Item"
           DISPLAY "3. Most Expensive Item"
           DISPLAY "0. Return to Main Menu"
           ACCEPT Choice
           EVALUATE Choice
           WHEN 1 PERFORM TopBrand
           WHEN 2 PERFORM CheapItem
           WHEN 3 PERFORM ExpensiveItem
           WHEN 0 GO TO MAIN
           WHEN OTHER GO TO ItemRecommend
           END-EVALUATE.
       *>  DISPLAYS THE CONTENT OF THE BRANDFILE SORTED FROM HIGHEST
       *>  NUMBER OF ITEMS SOLD TO THE LOWEST
       TopBrand.
           OPEN I-O BrandFile.
           OPEN OUTPUT TempFile2.
           DISPLAY SPACE
           DISPLAY "Top Brand Sold".
           DISPLAY SPACE
           DISPLAY "Brand Name        Items Sold"
           MOVE "N" TO WSEOF.
           PERFORM UNTIL WSEOF='Y'
               READ BrandFile NEXT RECORD INTO WSBrandData
                   AT END MOVE 'Y' TO WSEOF
                   NOT AT END
                   MOVE WSBrandData TO TFBrandData
                   WRITE TFBrandData
               END-WRITE
               END-READ
           END-PERFORM.
           CLOSE TempFile2.
           CLOSE BrandFile.
           SORT WorkFile2 ON DESCENDING KEY TFSold
               USING TempFile2
               GIVING TopBrandFile.
           OPEN INPUT TopBrandFile.
           MOVE "N" TO WSEOF.
           PERFORM UNTIL WSEOF='Y'
               READ TopBrandFile INTO TBBrandData
                   AT END MOVE 'Y' TO WSEOF
                   NOT AT END DISPLAY TBBrandData
           END-PERFORM.
           CLOSE TopBrandFile.
           GO TO ItemRecommend.
       *>  DISPLAYS THE TOP 5 ITEMS WITH THE LOWEST PRICE
       CheapItem.
           OPEN I-O InventoryFile.
           OPEN OUTPUT TempFile.
           DISPLAY " Cheapest Item List".
           MOVE "N" TO WSEOF.
           PERFORM UNTIL WSEOF='Y'
               READ InventoryFile NEXT RECORD INTO WSItemData
                   AT END MOVE 'Y' TO WSEOF
                   NOT AT END
                   MOVE WSItemName TO TItemName
                   MOVE WSPrice TO TPrice
                   WRITE TempItem
               END-WRITE
               END-READ
           END-PERFORM.
           CLOSE InventoryFile.
           CLOSE TempFile.
           SORT WorkFile ON ASCENDING KEY TPrice
               USING TempFile
               GIVING CheapFile.

           OPEN INPUT CheapFile.
           DISPLAY "Item Name           Price"
           MOVE "N" TO WSEOF.
           PERFORM 5 TIMES
               READ CheapFile INTO SortItem
               DISPLAY SortItem
           END-PERFORM.
           CLOSE CheapFile.
           GO TO ItemRecommend.
       *>  DISPLAYS THE TOP 5 ITEMS WITH THE HIGHEST PRICE
       ExpensiveItem.
           OPEN I-O InventoryFile.
           OPEN OUTPUT TempFile.
           DISPLAY " Most Expensive Item List".
           MOVE "N" TO WSEOF.
           PERFORM UNTIL WSEOF='Y'
               READ InventoryFile NEXT RECORD INTO WSItemData
                   AT END MOVE 'Y' TO WSEOF
                   NOT AT END
                   MOVE WSItemName TO TItemName
                   MOVE WSPrice TO TPrice
                   WRITE TempItem
                   END-WRITE
               END-READ
           END-PERFORM.
           CLOSE InventoryFile.
           CLOSE TempFile.
           SORT WorkFile ON DESCENDING KEY TPrice
               USING TempFile
               GIVING ExpensiveFile.

           OPEN INPUT ExpensiveFile.
           DISPLAY "Item Name           Price"
           PERFORM 5 TIMES
               READ ExpensiveFile INTO SortItem
               DISPLAY SortItem
           END-PERFORM.
           CLOSE ExpensiveFile.
           GO TO ItemRecommend.
