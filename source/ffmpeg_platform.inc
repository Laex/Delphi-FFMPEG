(* Cross-compiler platform normalization (Delphi + FPC).
   Included from ffmpeg.inc before library file names are defined. *)

{$IFDEF WINDOWS}
  {$IFNDEF MSWINDOWS}
    (* FPC on Win32 / Win64 defines WINDOWS but not MSWINDOWS. *)
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

{$IFDEF POSIX}
  {$IFNDEF LINUX}
    (* Delphi Linux64 *)
    {$DEFINE LINUX}
  {$ENDIF}
{$ENDIF}

(* Delay-load FFmpeg DLLs on Windows (packages/IDE can load .bpl without DLLs in path). *)
{$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
    {$DEFINE FF_DELAYED}
    {$WARN SYMBOL_PLATFORM OFF}
  {$ENDIF}
{$ENDIF}
