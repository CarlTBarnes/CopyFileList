
!Region Notices and Notes
! ================================================================================
! MIT License
! 
! Copyright (c) 2020 Mark Goldberg
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
! ===============================================================================================
!
!  Description  : A command line tool, to copy content from a FileList.xml to a folder
!  Created      : March 11, 2020
!  Last Updated : March 11, 2020
!
!  C:\> CopyFileList From=%FileList% To=%Folder% [Exclude="%CommaDelimWildcards"] [/SkipCommon]
!
! ===============================================================================================
!EndRegion Notices and Notes

! Possible Future Enhancements
! /NoMessage - remove calls to MESSAGE
!              failures will be HALT N, which can be detected by ERRORLEVEL in batch files
!
! Allow FROM to contain wildcards e.g. FROM=.\Obj\Debug\*.cwproj.FileList.XML to  do multiple projects
! Files names in XML are ALL UPPER. The COPY restult is UPPER. 
!    Would like Original Case. Maybe Use Directory() to get it.

 PROGRAM
 MAP
   Debug(STRING xMessage)
   ODS  (STRING xMessage)
   MODULE('API')
     OutputDebugString(*CSTRING),RAW,PASCAL,NAME('OutputDebugStringA'),DLL(1)
   END

   IsExcluded(STRING xFileName),BOOL
 END

 INCLUDE('StringTheory.inc'),ONCE
 INCLUDE('BigBangTheory.inc'),ONCE      !https://github.com/CarlTBarnes/StringTheory-LoadFile-Split-Viewer

Bang1       BigBangTheory 
BangDbg     BigBangTheory   !For ShowBang
ShowBang    BOOL(FALSE)     !Use /Bang to see BigBang Debug - Dumb name

ShowODS      BOOL(FALSE)

Common       EQUATE('*.ICO,*.OBJ,*.RSC,*.LIB,*.BMP,*.JPG,*.GIF,*.EXE,*.LNK,*.MANIFEST,*.RES,*.CUR')

FormalUsage  EQUATE('CopyFileList  From=%FileList%  To=%Folder%  [Exclude="%CommaDelimWildcards"]  [/SkipCommon] [/Debug] [/Bang]')
ExampleUsage EQUATE('CopyFileList From=C:\Example\Obj\Debug\MyProj.cwproj.FileList.xml To=C:\Tmp\Src2 Exclude="ab*.clw,ab*.inc" /SkipCommon')
Usage        EQUATE(FormalUsage & '||Example:|' & ExampleUsage & '||/SkipCommon will add:|' & Common)


InFile      ANY          ! Filename of FileList.xml
DestFolder  ANY          ! Target folder for the copy
Exclude     StringTheory ! Split list of WildCards to exclude from the copy
FileList    StringTheory ! LoadFile of the FlieList.xml
DoneListCopy    StringTheory ! List of Files Copied
DoneListSkip    StringTheory ! List of Files Skipped
DoneListErrs    StringTheory ! List of Files Errored

!==========================================================================================
 CODE
 SYSTEM{7A7Dh}=1 !PROP:MsgModeDefault added C10 or 11 so all messages allow copy text
 Bang1.LineViewInConsolas = TRUE
 BangDbg.LineViewInConsolas = TRUE
 ShowBang    = CHOOSE( UPPER(COMMAND('/Bang'))  = 'BANG')  ! controls if BigBang Windows show

 ShowODS     = CHOOSE( UPPER(COMMAND('/Debug')) = 'DEBUG') ! controls if Debug messages are sent to OutputDebugString
 InFile      = COMMAND('From')
 DestFolder  = COMMAND('To')
 
                       ! ODS('ShowODS['& ShowODS &']')
                       Debug('Infile['& Infile &']')
                       Debug('DestFolder['& DestFolder &']')

 IF ~InFile OR ~DestFolder
     MESSAGE('Command line requires /FROM= and /TO= Parameters||Note: /FROM should be a FileList.XML' & |
             '||Usage:|' & Usage &'||Command:|' & COMMAND('') ,'CopyFileList Parameters',,'Close',,MSGMODE:CANCOPY)
     HALT(1)
 END
 
 IF ~EXISTS( InFile )
     MESSAGE('From['& InFile &']|Not found|Note: /FROM should be a FileList.XML||Usage:|' & Usage  ,'CopyFileList did NOT Copy')
     HALT(1)
 END

 IF ~EXISTS(DestFolder)
     MESSAGE('To Folder['& DestFolder &']|Not found||Usage:|' & Usage,'CopyFileList did NOT Copy')
     HALT(2) 
 END 

 IF ~FileList.LoadFile( InFile ) THEN       !06/23/22 Carl added IF Failed and Msg
    Message('FileList LoadFile failed' & |
            '||Error: '& FileList.winErrorCode &' '& FileList.LastError & |
            '||File: ' & InFile,'CopyFileList')
    HALT(1)            
 END
 IF ShowBang THEN BangDbg.ValueView(FileList,'FileList '& InFile).
 
 FileList.Split('<13,10>')
 IF ShowBang THEN BangDbg.LinesViewInList(FileList,'FileList Lines '& InFile).

 IF CLIP(LEFT(FileList.GetLine(2))) <>  '<<Opened_Files>'
    CASE MESSAGE('Expected the 2nd line to be <<Opened_Files>' & |
                 '|It was: ' & FileList.GetLine(2) & |
                 '||File: ' & CLIP(InFile) , |
                 'CopyFileList did NOT Copy',,'Close|View File')
    OF 2 ; Bang1.DoNotShow=False 
           Bang1.LinesViewInList(FileList,'FileList Lines '& InFile) 
    END           
    HALT(3)
 END

 DO SetExclude
 DO CopyNow
 RETURN     ! think HALT 0

!---------------------------------
SetExclude                 ROUTINE 
 Exclude.SetValue( UPPER(COMMAND('Exclude')))
 Exclude.Split(',')

 IF COMMAND('/SkipCommon')
    IF Exclude.Records() = 0 
       Exclude.SetValue( Common )
       Exclude.Split(',')
    ELSE 
       DO SetExclude:MergeCommonExclusions
    END 
 END
 IF ShowBang AND Exclude.Records() THEN 
    BangDbg.LinesViewInList(Exclude,'Exclude List' & CHOOSE(~COMMAND('/SkipCommon'),'',' /SkipCommon') )
 END
                             Debug('Exclude.GetValue()['& Exclude.GetValue() &'] Exclude.Records()['& Exclude.Records() &']')
                             LOOP Tmp# = 1 TO Exclude.Records()
                               Debug('Exclude['& Tmp# &'] ' & Exclude.GetLine(Tmp#) )
                             END 

!---------------------------------
SetExclude:MergeCommonExclusions      ROUTINE 
  DATA
ExclusionsToAdd   ANY
ST:Common         StringTheory
CommonRowNum      LONG,AUTO
ExcludeRowNum     LONG,AUTO
Found             BOOL,AUTO
CurrCommon        ANY
  CODE 
  ST:Common.SetValue( Common )
  ST:Common.Split(',')
 
  LOOP CommonRowNum = 1 TO ST:Common.Records()
     CurrCommon = ST:Common.GetLine( CommonRowNum )

     Found=FALSE
     LOOP ExcludeRowNum = 1 TO Exclude.Records()
        IF Exclude.GetLine( ExcludeRowNum) = CurrCommon
           Found=TRUE
           BREAK
        END 
     END 
     IF ~Found 
        ExclusionsToAdd= ExclusionsToAdd & ',' & CurrCommon   ! the ',' on the first entry is just fine (in fact its required)
     END 
  END  

  Exclude.Append( ExclusionsToAdd )
  Exclude.Split(',')

   
!---------------------------------
CopyNow                    ROUTINE 
 DATA 
RowNum         LONG,AUTO
ShouldCopyFile BOOL
CurrRow        StringTheory
Count:Err      LONG(0)
Count:Success  LONG(0)
Count:Skip     LONG(0)
FileName       ANY 
 CODE 

 LOOP RowNum = 3 TO FileList.Records()
   CurrRow.SetValue ( FileList.GetLine( RowNum ) )
   IF CLIP(LEFT(CurrRow.GetValue()))= '<</Opened_Files>'
      BREAK 
   END 

   ! example expected content: <file name="C:\SV\CLARION11.0.13244\LIBSRC\WIN\PROPERTY.CLW" />
   FileName = CurrRow.Between('"', '"', , , st:NoCase )
                                         !Debug('FileName['& FileName &']')
   
   IF FileName=''                   
      !blank
   ELSIF IsExcluded( FileName) 
      Debug('Skipping FileName['& FileName &']')
      Count:Skip += 1
      DoneListSkip.AddLine(FileName)
   ELSE 
      COPY( FileName, DestFolder)
      IF ErrorCode() 
         Count:Err += 1
         DoneListErrs.AddLine('Error: ' & ErrorCode() &' - '& CLIP(FileName) &' - '& ERROR())
      ELSE 
         Count:Success += 1
         DoneListCopy.AddLine(FileName)
      END 
      Debug('FileName['& FileName &'] ErrorCode()['& ErrorCode() &']=['& Error() &']')      
   END 
 END

    IF ~DoneListCopy.Records() THEN DoneListCopy.AddLine('(None)'). ; DoneListCopy.Join('<13,10>')
    IF ~DoneListSkip.Records() THEN DoneListSkip.AddLine('(None)'). ; DoneListSkip.Join('<13,10>')
    IF ~DoneListErrs.Records() THEN DoneListErrs.AddLine('(None)'). ; DoneListErrs.Join('<13,10>')
    IF ~Exclude.Records() THEN Exclude.AddLine('(None)'). ; Exclude.Join('<13,10>')

 LOOP 
    Bang1.DoNotShow=False
    CASE MESSAGE(  'Count:Success <9>[ '& Count:Success &' ]' |
                & '|Count:Errors  <9>[ '& Count:Err     &' ]' |
                & '|Count:Skipped <9>[ '& Count:Skip    &' ]' |
                & '||Copy From: '  & CLIP(InFile)     |
                & '|Destination: ' & CLIP(DestFolder) |
                & '||Command: ' & COMMAND('')         |
                ,'Done CopyFileList'                  |
                ,                                     |
                ,'&Close|Explore Dest|View Copys|View Errors|Skipped Files|Skip List')
    OF 1 ; BREAK                
    OF 2 ; RUN('Explorer "' & CLIP(DestFolder) &'"')
    OF 3 ; Bang1.LinesViewInList(DoneListCopy)
    OF 4 ; Bang1.LinesViewInList(DoneListErrs)
    OF 5 ; Bang1.LinesViewInList(DoneListSkip)
    OF 6 ; Bang1.LinesViewInList(Exclude)
    END
    CYCLE
 END  

!=========================================
IsExcluded PROCEDURE(STRING xFileName) ! assumes xFileName is upper cased
ExcludeRowNum  LONG,AUTO
RetIncluded    BOOL(TRUE)
  CODE 
   LOOP ExcludeRowNum = 1 TO Exclude.Records()
      ! Tests show that Match:Wild is case sensitive, 
      ! so      I did an UPPER on Exclude above
      ! and I will ASSUME that the xFileName is already upper case
      !   as that's how a FileList.xml appears to be written.
      
                                       ! Debug('Match checking Exclude.GetLine(ExcludeRowNum)['& Exclude.GetLine(ExcludeRowNum) &']')

      IF MATCH( xFileName, Exclude.GetLine( ExcludeRowNum ), Match:Wild)
         RETURN TRUE
      END 
   END 

   RETURN FALSE
!=========================================
Debug PROCEDURE(STRING xMessage)
  CODE 
  IF ShowODS
     ODS(xMessage)
  END 
!=========================================
ODS   PROCEDURE(STRING xMessage)
sz  &CSTRING
  CODE 
  sz &= NEW CSTRING( SIZE(xMessage) + 1)
  sz  = xMessage
  OutputDebugString( sz )
  DISPOSE(sz)
