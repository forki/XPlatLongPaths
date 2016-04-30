namespace XPlatLongPaths.IO

type SystemIOPath           = System.IO.Path
type SystemIOFile           = System.IO.File
type SystemIODirectory      = System.IO.Directory
type SystemIOFileSystemInfo = System.IO.FileSystemInfo
type SystemIODirectoryInfo  = System.IO.DirectoryInfo
type SystemIOFileInfo       = System.IO.FileInfo

type OtherPath              = Alphaleonis.Win32.Filesystem.Path
type OtherFile              = Alphaleonis.Win32.Filesystem.File
type OtherDirectory         = Alphaleonis.Win32.Filesystem.Directory
type OtherFileSystemInfo    = Alphaleonis.Win32.Filesystem.FileSystemInfo
type OtherDirectoryInfo     = Alphaleonis.Win32.Filesystem.DirectoryInfo
type OtherFileInfo          = Alphaleonis.Win32.Filesystem.FileInfo

module internal Internals =
  let runningOnWindows = System.Environment.OSVersion.Platform = System.PlatformID.Win32NT
  let inline doEither a b = if runningOnWindows then a () else b ()
  let inline (=>) a f = fun () -> f a // sorry
open Internals

type [<AllowNullLiteral>] FileSystemInfo internal (systemIO: Lazy<SystemIOFileSystemInfo>, other: Lazy<OtherFileSystemInfo>) =
  
  internal new (file: OtherFileSystemInfo)    = FileSystemInfo(lazy null, lazy file)
  internal new (file: SystemIOFileSystemInfo) = FileSystemInfo(lazy file, lazy null)

  static member internal MakeFromSystemIO(f:SystemIOFileSystemInfo) =
    match f with
    | :? SystemIOFileInfo      as f -> FileInfo(f)      :> FileSystemInfo
    | :? SystemIODirectoryInfo as d -> DirectoryInfo(d) :> FileSystemInfo

  static member internal MakeFromOther(f:OtherFileSystemInfo) =
    match f with
    | :? OtherFileInfo      as f -> FileInfo(f)      :> FileSystemInfo
    | :? OtherDirectoryInfo as d -> DirectoryInfo(d) :> FileSystemInfo
  
  member x.FullName =
    if runningOnWindows then other.Value.FullName
    else systemIO.Value.FullName

  member x.Name =
    if runningOnWindows then other.Value.Name
    else systemIO.Value.Name

  member x.Exists =
    if runningOnWindows then other.Value.Exists
    else systemIO.Value.Exists

  member x.Attributes
    with get () =
      if runningOnWindows then other.Value.Attributes else systemIO.Value.Attributes
    and set(value) =
      if runningOnWindows then other.Value.Attributes <- value else systemIO.Value.Attributes <- value
  
  member x.Delete () =
    doEither other.Value.Delete systemIO.Value.Delete

and FileInfo internal (systemIO: Lazy<SystemIOFileInfo>, other:Lazy<OtherFileInfo>) =
  inherit FileSystemInfo(lazy (systemIO.Value :> _), lazy (other.Value :> _))
  
  internal new (file: OtherFileInfo)    = FileInfo(lazy null, lazy file)
  internal new (file: SystemIOFileInfo) = FileInfo(lazy file, lazy null)
  
  new (fullPath: string) = FileInfo(lazy SystemIOFileInfo(fullPath), lazy OtherFileInfo(fullPath))

  member x.Directory =
    if runningOnWindows then other.Value.Directory |> DirectoryInfo
    else systemIO.Value.Directory |> DirectoryInfo
  member x.Extension =
    if runningOnWindows then other.Value.Extension
    else systemIO.Value.Extension

  member x.Length =
    if runningOnWindows then other.Value.Length
    else systemIO.Value.Length

  member x.Delete () =
    doEither other.Value.Delete systemIO.Value.Delete
  
  member x.OpenRead () =
    doEither other.Value.OpenRead systemIO.Value.OpenRead

  member x.CopyTo (destination: string, overwrite: bool) =
    doEither
      ((destination, overwrite) => other.Value.CopyTo    >> FileInfo)
      ((destination, overwrite) => systemIO.Value.CopyTo >> FileInfo)

and [<AllowNullLiteral>] DirectoryInfo internal (systemIO : Lazy<SystemIODirectoryInfo>, other: Lazy<OtherDirectoryInfo>) =
  inherit FileSystemInfo(lazy (systemIO.Value :> _), lazy (other.Value :> _))
  
  internal new (directory: OtherDirectoryInfo)    = DirectoryInfo(lazy null, lazy directory)
  internal new (directory: SystemIODirectoryInfo) = DirectoryInfo(lazy directory, lazy null)
  
  new (fullPath:string) = DirectoryInfo(lazy SystemIODirectoryInfo(fullPath), lazy OtherDirectoryInfo(fullPath))
  
  member x.EnumerateFiles () =
     doEither 
      (other.Value.EnumerateFiles    >> Seq.map FileInfo)
      (systemIO.Value.EnumerateFiles >> Seq.map FileInfo)
    
  member x.EnumerateFiles (searchPattern) =
    doEither
      (searchPattern => other.Value.EnumerateFiles    >> Seq.map FileInfo)
      (searchPattern => systemIO.Value.EnumerateFiles >> Seq.map FileInfo)

  member x.EnumerateDirectories () =
     doEither
      (other.Value.EnumerateDirectories    >> Seq.map DirectoryInfo)
      (systemIO.Value.EnumerateDirectories >> Seq.map DirectoryInfo)
    
  member x.GetDirectories () =
     doEither
      (other.Value.GetDirectories    >> Array.map DirectoryInfo)
      (systemIO.Value.GetDirectories >> Array.map DirectoryInfo)
       
  member x.GetDirectories searchPattern =
     doEither
      (searchPattern => other.Value.GetDirectories    >> Array.map DirectoryInfo)
      (searchPattern => systemIO.Value.GetDirectories >> Array.map DirectoryInfo)
      
  member x.GetFiles () =
     doEither
      (other.Value.GetFiles    >> Array.map FileInfo)
      (systemIO.Value.GetFiles >> Array.map FileInfo)
        
  member x.GetFiles (pattern: string) =
     doEither
      (pattern => other.Value.GetFiles    >> Array.map FileInfo)
      (pattern => systemIO.Value.GetFiles >> Array.map FileInfo)
          
  member x.GetFiles (pattern: string, searchOption) =
     doEither
      ((pattern, searchOption) => other.Value.GetFiles    >> Array.map FileInfo)
      ((pattern, searchOption) => systemIO.Value.GetFiles >> Array.map FileInfo)
  
  member x.EnumerateFileSystemInfos () =
     doEither
      (other.Value.EnumerateFileSystemInfos    >> Seq.map FileSystemInfo.MakeFromOther)
      (systemIO.Value.EnumerateFileSystemInfos >> Seq.map FileSystemInfo.MakeFromSystemIO)
        
  member x.CreateSubdirectory (path: string) =
     doEither
      (path => other.Value.CreateSubdirectory    >> DirectoryInfo)
      (path => systemIO.Value.CreateSubdirectory >> DirectoryInfo)
             
  member x.Create () =
    doEither other.Value.Create systemIO.Value.Create

  member x.Parent =
    if runningOnWindows then 
      if isNull other.Value.Parent then null
      else (other.Value.Parent |> DirectoryInfo)
    else 
      if isNull systemIO.Value.Parent then null
      else (systemIO.Value.Parent |> DirectoryInfo)

type File =
  
  static member SetLastWriteTimeUtc (filename, date) =
    doEither
      ((filename, date) => OtherFile.SetLastWriteTimeUtc)
      ((filename, date) => SystemIOFile.SetLastWriteTimeUtc)
  
  static member AppendAllLines (filename, lines) =
    doEither
      ((filename, lines) => OtherFile.AppendAllLines)
      ((filename, lines) => SystemIOFile.AppendAllLines)

  static member Copy (source, target) =
    doEither
      ((source, target) => OtherFile.Copy)
      ((source, target) => SystemIOFile.Copy)

  static member Copy (source, target, overwrite: bool) =
    doEither
      ((source, target, overwrite) => OtherFile.Copy)
      ((source, target, overwrite) => SystemIOFile.Copy)

  static member Exists filename =
    doEither
      (filename => OtherFile.Exists)
      (filename => SystemIOFile.Exists)
  
  static member ReadAllText filename =
    doEither
      (filename => OtherFile.ReadAllText)
      (filename => SystemIOFile.ReadAllText)
    
  static member ReadAllBytes filename =
    doEither
      (filename => OtherFile.ReadAllBytes)
      (filename => SystemIOFile.ReadAllBytes)
  
  static member WriteAllText (filename, text) =
    doEither
      ((filename,text) => OtherFile.WriteAllText)
      ((filename,text) => SystemIOFile.WriteAllText)

  static member Open (name, fileMode) =
    doEither
      ((name, fileMode) => OtherFile.Open)
      ((name, fileMode) => SystemIOFile.Open)

  static member Delete name =
    doEither
      (name => OtherFile.Delete)
      (name => SystemIOFile.Delete)


type Directory =

  static member CreateDirectory name =
    doEither
      (name => OtherDirectory.CreateDirectory    >> DirectoryInfo)
      (name => SystemIODirectory.CreateDirectory >> DirectoryInfo)

  static member Exists name =
    doEither
      (name => OtherDirectory.Exists)
      (name => SystemIODirectory.Exists)

  static member EnumerateFiles (path: string, pattern, searchOption) =
    doEither
      ((path, pattern, searchOption) => OtherDirectory.EnumerateFiles)
      ((path, pattern, searchOption) => SystemIODirectory.EnumerateFiles)
  
  static member GetFiles (path: string, pattern) =
    doEither
      ((path, pattern) => OtherDirectory.GetFiles)
      ((path, pattern) => SystemIODirectory.GetFiles)
  
  static member EnumerateDirectories (path: string, pattern, searchOption) =
    doEither
      ((path, pattern, searchOption) => OtherDirectory.EnumerateDirectories)
      ((path, pattern, searchOption) => SystemIODirectory.EnumerateDirectories)
  
type Path =
  
  static member GetDirectoryName fullPath =
    doEither
      (fullPath => OtherPath.GetDirectoryName)
      (fullPath => SystemIOPath.GetDirectoryName)

  static member GetExtension fullPath =
    doEither
      (fullPath => OtherPath.GetExtension)
      (fullPath => SystemIOPath.GetExtension)

  static member IsPathRooted path =
    doEither
      (path => OtherPath.IsPathRooted)
      (path => SystemIOPath.IsPathRooted)
  
  static member GetFileNameWithoutExtension filename =
    doEither
      (filename => OtherPath.GetFileNameWithoutExtension)
      (filename => SystemIOPath.GetFileNameWithoutExtension)

  static member GetFileName filename =
    doEither
      (filename => OtherPath.GetFileName)
      (filename => SystemIOPath.GetFileName)

  static member GetFullPath path =
    doEither
      (path => OtherPath.GetFullPath)
      (path => SystemIOPath.GetFullPath)

  static member Combine ([<System.ParamArray>]paths) =
    doEither
      (fun () -> OtherPath.Combine(paths))
      (fun () -> SystemIOPath.Combine(paths))
  
  static member DirectorySeparatorChar =
    if runningOnWindows then OtherPath.DirectorySeparatorChar else SystemIOPath.DirectorySeparatorChar