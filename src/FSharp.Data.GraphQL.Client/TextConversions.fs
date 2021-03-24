// --------------------------------------------------------------------------------------
// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind.
// We disclaim all warranties, either express or implied, including the
// warranties of merchantability and fitness for a particular purpose.
// A simple F# portable parser for JSON data
// --------------------------------------------------------------------------------------

namespace FSharp.Data.GraphQL.Client

open System
open System.Globalization
open System.Text.RegularExpressions

[<AutoOpen>]
module private TextConversionHelpers =
  let asOption = function true, v -> Some v | _ -> None

  let (|StringEqualsIgnoreCase|_|) (s1:string) s2 =
    if s1.Equals(s2, StringComparison.OrdinalIgnoreCase)
      then Some () else None

  let (|OneOfIgnoreCase|_|) set str =
    if Array.exists (fun s -> StringComparer.OrdinalIgnoreCase.Compare(s, str) = 0) set then Some() else None

  let msDateRegex = lazy Regex(@"^/Date\((-?\d+)([-+]\d+)?\)/$", RegexOptions.Compiled)

  let dateTimeStyles = DateTimeStyles.AllowWhiteSpaces ||| DateTimeStyles.RoundtripKind

  let ParseISO8601FormattedDateTime (text:string) cultureInfo =
    match DateTime.TryParse(text, cultureInfo, dateTimeStyles) with
    | true, d -> d |> Some
    | false, _ -> None

type TextConversions private() =
  static member val DefaultMissingValues = [| "NaN"; "NA"; "N/A"; "#N/A"; ":"; "-"; "TBA"; "TBD" |]

  static member val DefaultNonCurrencyAdorners = [| '%'; '‰'; '‱' |] |> Set.ofArray

  static member val DefaultCurrencyAdorners = [| '¤'; '$'; '¢'; '£'; '¥'; '₱'; '﷼'; '₤'; '₭'; '₦'; '₨'; '₩'; '₮'; '€'; '฿'; '₡'; '៛'; '؋'; '₴'; '₪'; '₫'; '₹'; 'ƒ' |] |> Set.ofArray

  static member val private DefaultRemovableAdornerCharacters =
    Set.union TextConversions.DefaultNonCurrencyAdorners TextConversions.DefaultCurrencyAdorners

  static member private RemoveAdorners (value : string) =
    String(value.ToCharArray() |> Array.filter (not << TextConversions.DefaultRemovableAdornerCharacters.Contains))

  static member AsString str =
    if String.IsNullOrWhiteSpace str then None else Some str

  static member AsInteger cultureInfo text =
    Int32.TryParse(TextConversions.RemoveAdorners text, NumberStyles.Integer, cultureInfo) |> asOption

  static member AsFloat missingValues useNoneForMissingValues cultureInfo (text:string) =
    match text.Trim() with
    | OneOfIgnoreCase missingValues -> if useNoneForMissingValues then None else Some Double.NaN
    | _ -> Double.TryParse(text, NumberStyles.Any, cultureInfo)
           |> asOption
           |> Option.bind (fun f -> if useNoneForMissingValues && Double.IsNaN f then None else Some f)

  static member AsBoolean (text:string) =
    match text.Trim() with
    | StringEqualsIgnoreCase "true" | StringEqualsIgnoreCase "yes" | StringEqualsIgnoreCase "1" -> Some true
    | StringEqualsIgnoreCase "false" | StringEqualsIgnoreCase "no" | StringEqualsIgnoreCase "0" -> Some false
    | _ -> None

  static member AsDateTime cultureInfo (text:string) =
    let matchesMS = msDateRegex.Value.Match (text.Trim())
    if matchesMS.Success then
      matchesMS.Groups.[1].Value
      |> Double.Parse
      |> DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).AddMilliseconds
      |> Some
    else
      match ParseISO8601FormattedDateTime text cultureInfo with
      | Some d when d.Kind = DateTimeKind.Unspecified -> new DateTime(d.Ticks, DateTimeKind.Local) |> Some
      | x -> x

  static member AsDateTimeOffset cultureInfo (text:string) =
    let getTimeSpanFromHourMin (hourMin:int) =
      let hr = (hourMin/100) |> float |> TimeSpan.FromHours
      let min = (hourMin%100) |> float |> TimeSpan.FromMinutes
      hr.Add min

    let offset (str:string) =
      match Int32.TryParse str with
      | true, v -> getTimeSpanFromHourMin v |> Some
      | false, _ -> None

    let matchesMS = msDateRegex.Value.Match (text.Trim())
    if matchesMS.Success && matchesMS.Groups.[2].Success && matchesMS.Groups.[2].Value.Length = 5 then
      match offset matchesMS.Groups.[2].Value with
      | Some ofst ->
        matchesMS.Groups.[1].Value
        |> Double.Parse
        |> DateTimeOffset(1970, 1, 1, 0, 0, 0, ofst).AddMilliseconds
        |> Some
      | None -> None
    else
      match ParseISO8601FormattedDateTime text cultureInfo with
      | Some d when d.Kind <> DateTimeKind.Unspecified ->
        match DateTimeOffset.TryParse(text, cultureInfo, dateTimeStyles) with
        | true, dto -> dto |> Some
        | false, _ -> None
      | _ -> None

  static member AsTimeSpan (cultureInfo: CultureInfo) (text:string) =
    match TimeSpan.TryParse(text, cultureInfo) with
    | true, t -> Some t
    | _ -> None

  static member AsGuid (text:string) =
    Guid.TryParse(text.Trim()) |> asOption

module internal UnicodeHelper =
    let getUnicodeSurrogatePair num =
        let codePoint = num - 0x010000u
        let HIGH_TEN_BIT_MASK = 0xFFC00u
        let LOW_TEN_BIT_MASK = 0x003FFu
        let leadSurrogate = (codePoint &&& HIGH_TEN_BIT_MASK >>> 10) + 0xD800u
        let trailSurrogate = (codePoint &&& LOW_TEN_BIT_MASK) + 0xDC00u
        char leadSurrogate, char trailSurrogate