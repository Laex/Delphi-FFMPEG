{$POINTERMATH ON}
{$MINENUMSIZE 4} (* use 4-byte enums *)
{$WRITEABLECONST ON}

  (*
    * Version constants synced with FFmpeg n8.1.1 (libav*/version.h).
    * DLL file names use VERSION_MAJOR only (stable across minor bumps).
    * Runtime: BtbN shared build ffmpeg-n8.1-latest (see bin/scripts/download_ffmpeg_dll.ps1).
  *)
const
{$REGION 'libavutil'}
  LIBAVUTIL_VERSION_MAJOR     = 60;
  LIBAVUTIL_VERSION_MAJOR_STR = '60';
  LIBAVUTIL_VERSION_MINOR     = 26;
  LIBAVUTIL_VERSION_MICRO     = 101;
  FFMPEG_VERSION              = '8.1.1';
  LIBAVUTIL_VERSION_INT     = ((LIBAVUTIL_VERSION_MAJOR shl 16) or (LIBAVUTIL_VERSION_MINOR shl 8) or LIBAVUTIL_VERSION_MICRO);
{$ENDREGION}

{$REGION 'libswscale'}
  LIBSWSCALE_VERSION_MAJOR     = 9;
  LIBSWSCALE_VERSION_MAJOR_STR = '9';
  LIBSWSCALE_VERSION_MINOR     = 5;
  LIBSWSCALE_VERSION_MICRO     = 101;
{$ENDREGION}

{$REGION 'libavcodec'}
  LIBAVCODEC_VERSION_MAJOR     = 62;
  LIBAVCODEC_VERSION_MAJOR_STR = '62';
  LIBAVCODEC_VERSION_MINOR     = 28;
  LIBAVCODEC_VERSION_MICRO     = 101;
{$ENDREGION}

{$REGION 'avdevice'}
  LIBAVDEVICE_VERSION_MAJOR     = 62;
  LIBAVDEVICE_VERSION_MAJOR_STR = '62';
  LIBAVDEVICE_VERSION_MINOR     = 3;
  LIBAVDEVICE_VERSION_MICRO     = 101;
{$ENDREGION}

{$REGION 'avformat'}
  LIBAVFORMAT_VERSION_MAJOR     = 62;
  LIBAVFORMAT_VERSION_MAJOR_STR = '62';
  LIBAVFORMAT_VERSION_MINOR     = 12;
  LIBAVFORMAT_VERSION_MICRO     = 101;
{$ENDREGION}

{$REGION 'postproc'}
  (* Legacy optional library; not built from FFmpeg 8.x source tree. *)
  LIBPOSTPROC_VERSION_MAJOR     = 58;
  LIBPOSTPROC_VERSION_MAJOR_STR = '58';
  LIBPOSTPROC_VERSION_MINOR     = 3;
  LIBPOSTPROC_VERSION_MICRO     = 100;
{$ENDREGION}

{$REGION 'swresample.h'}
  LIBSWRESAMPLE_VERSION_MAJOR     = 6;
  LIBSWRESAMPLE_VERSION_MAJOR_STR = '6';
  LIBSWRESAMPLE_VERSION_MINOR     = 3;
  LIBSWRESAMPLE_VERSION_MICRO     = 101;
{$ENDREGION}

{$REGION 'avfilter.h'}
  LIBAVFILTER_VERSION_MAJOR     = 11;
  LIBAVFILTER_VERSION_MAJOR_STR = '11';
  LIBAVFILTER_VERSION_MINOR     = 14;
  LIBAVFILTER_VERSION_MICRO     = 101;
{$ENDREGION}

(* libavutil/version.h (n8.1.1) *)
{$IF LIBAVUTIL_VERSION_MAJOR < 61}
  {$DEFINE FF_API_MOD_UINTP2}
  {$DEFINE FF_API_RISCV_FD_ZBA}
  {$DEFINE FF_API_VULKAN_FIXED_QUEUES}
  {$DEFINE FF_API_OPT_INT_LIST}
  {$DEFINE FF_API_OPT_PTR}
  {$DEFINE FF_API_CPU_FLAG_FORCE}
  {$DEFINE FF_API_DOVI_L11_INVALID_PROPS}
  {$DEFINE FF_API_ASSERT_FPU}
{$IFEND}
{$IF LIBAVUTIL_VERSION_MAJOR < 62}
  {$DEFINE FF_API_VULKAN_SYNC_QUEUES}
{$IFEND}

(* libavcodec/version_major.h (n8.1.1) *)
{$IF LIBAVCODEC_VERSION_MAJOR < 63}
  {$DEFINE FF_API_INIT_PACKET}
  {$DEFINE FF_API_V408_CODECID}
  {$DEFINE FF_API_CODEC_PROPS}
  {$DEFINE FF_API_EXR_GAMMA}
  {$DEFINE FF_API_INTRA_DC_PRECISION}
  {$DEFINE FF_API_NVDEC_OLD_PIX_FMTS}
  {$DEFINE FF_API_PARSER_PRIVATE}
  {$DEFINE FF_API_PARSER_CODECID}
  {$DEFINE FF_CODEC_OMX}
  {$DEFINE FF_CODEC_SONIC_ENC}
  {$DEFINE FF_CODEC_SONIC_DEC}
{$IFEND}

(* libavformat/version_major.h (n8.1.1) *)
{$IF LIBAVFORMAT_VERSION_MAJOR < 63}
  {$DEFINE FF_API_COMPUTE_PKT_FIELDS2}
  {$DEFINE FF_API_INTERNAL_TIMING}
  {$DEFINE FF_API_NO_DEFAULT_TLS_VERIFY}
{$IFEND}
{$DEFINE FF_API_R_FRAME_RATE}

(* libavfilter/version_major.h (n8.1.1) *)
{$IF LIBAVFILTER_VERSION_MAJOR < 12}
  {$DEFINE FF_API_BUFFERSINK_OPTS}
  {$DEFINE FF_API_CONTEXT_PUBLIC}
  {$DEFINE FF_API_LIBNPP_SUPPORT}
{$IFEND}

(* libavdevice/version_major.h (n8.1.1) *)
{$IF LIBAVDEVICE_VERSION_MAJOR < 63}
  {$DEFINE FF_API_ALSA_CHANNELS}
{$IFEND}

const
{$IFDEF MSWINDOWS}
  swscale_dll    = 'swscale-' + LIBSWSCALE_VERSION_MAJOR_STR + '.dll';
  avutil_dll     = 'avutil-' + LIBAVUTIL_VERSION_MAJOR_STR + '.dll';
  avcodec_dll    = 'avcodec-' + LIBAVCODEC_VERSION_MAJOR_STR + '.dll';
  avdevice_dll   = 'avdevice-' + LIBAVDEVICE_VERSION_MAJOR_STR + '.dll';
  avformat_dll   = 'avformat-' + LIBAVFORMAT_VERSION_MAJOR_STR + '.dll';
  postproc_dll   = 'postproc-' + LIBPOSTPROC_VERSION_MAJOR_STR + '.dll';
  swresample_dll = 'swresample-' + LIBSWRESAMPLE_VERSION_MAJOR_STR + '.dll';
  avfilter_dll   = 'avfilter-' + LIBAVFILTER_VERSION_MAJOR_STR + '.dll';
{$ENDIF}

{$IFDEF LINUX}
  swscale_dll    = 'libswscale.so.' + LIBSWSCALE_VERSION_MAJOR_STR;
  avutil_dll     = 'libavutil.so.' + LIBAVUTIL_VERSION_MAJOR_STR;
  avcodec_dll    = 'libavcodec.so.' + LIBAVCODEC_VERSION_MAJOR_STR;
  avdevice_dll   = 'libavdevice.so.' + LIBAVDEVICE_VERSION_MAJOR_STR;
  avformat_dll   = 'libavformat.so.' + LIBAVFORMAT_VERSION_MAJOR_STR;
  postproc_dll   = 'libpostproc.so.' + LIBPOSTPROC_VERSION_MAJOR_STR;
  swresample_dll = 'libswresample.so.' + LIBSWRESAMPLE_VERSION_MAJOR_STR;
  avfilter_dll   = 'libavfilter.so.' + LIBAVFILTER_VERSION_MAJOR_STR;
{$ENDIF}

{$IFDEF ANDROID}
  swscale_dll    = 'libswscale.so';
  avutil_dll     = 'libavutil.so';
  avcodec_dll    = 'libavcodec.so';
  avdevice_dll   = 'libavdevice.so';
  avformat_dll   = 'libavformat.so';
  postproc_dll   = 'libpostproc.so';
  swresample_dll = 'libswresample.so';
  avfilter_dll   = 'libavfilter.so';
{$ENDIF}
