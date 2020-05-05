module StringToArray exposing (..)

import Array exposing (Array)


type alias CsvText =
    String


type alias MarkdownText =
    String


type alias Array2D =
    Array (List String)


type alias List2D =
    List (List String)



-- CSV
{-
   csv -> array
   1. 改行コードで分割
   2. カンマで分割、で行ごとに配列化
-}


csvToArrayList : CsvText -> Array2D
csvToArrayList csv =
    csv
        |> String.lines
        |> List.map (String.split ",")
        |> Array.fromList



{-
   array -> csv
-}


arrayListToCsv : Array2D -> CsvText
arrayListToCsv array2d =
    array2d
        |> Array.toList
        |> convertList2DToCsv



{-
   各配列要素の末尾に改行コードを付加
   1. 各要素の配列をcsv文字列化 (,でjoin)
   2. 各要素の末尾に改行コードを追加
   3. 配列要素をconcatで連結
-}


convertList2DToCsv : List2D -> String
convertList2DToCsv list2d =
    let
        list =
            list2d
                |> List.map (String.join ",")

        text =
            list
                |> List.map appendLineBreak
                |> String.concat
    in
    text



{-
   文字列の末尾に改行コードを付加
   - 文字数 = 0の場合は空文字を返す
-}


appendLineBreak : String -> String
appendLineBreak text =
    case String.length text of
        0 ->
            ""

        _ ->
            String.append text "\n"



-- MARKDOWN
{-
   markdown -> array
   1. 改行コードで分割
   2. markdown用のフォーマット (:--)が存在する行を落とす
   3. 先頭と末尾の"|"を削除
   4. "|"で分割、で行ごとに配列化
-}


markdownToArrayList : MarkdownText -> Array2D
markdownToArrayList markdown =
    markdown
        |> String.lines
        |> List.filter notMarkdownFormat
        |> List.map splitStringByVerticalBar
        |> Array.fromList



{-
   markdown文字列の中に":--"が含まれているかどうか
   含まれていないパターンを残したいので
   - ":--"が含まれている場合 -> False
   - ":--"が含まれていない場合 -> True
-}


notMarkdownFormat : String -> Bool
notMarkdownFormat text =
    let
        isFormatContained =
            text
                |> String.split "|"
                |> List.member markdownTextAlignLeft
    in
    not isFormatContained



{-
   markdownのtext-align: left
-}


markdownTextAlignLeft : String
markdownTextAlignLeft =
    ":--"



{-
   array -> markdown
-}


arrayListToMarkdown : Array2D -> MarkdownText
arrayListToMarkdown array2d =
    array2d
        |> Array.toList
        |> convertList2DToMarkdown



{-
   2次元配列をmarkdownフォーマットに変換
   1. 2d -> 1d変換、2次元の各要素は|で連結
   2. 2行目に左右のtext align用フォーマットを追加
   3. 残りの要素と連結
-}


convertList2DToMarkdown : List2D -> MarkdownText
convertList2DToMarkdown list2d =
    let
        -- 2d -> 1d：|で連結
        list =
            list2d
                |> List.map (String.join "|")

        -- 最初の配列要素の、要素数
        n =
            List.take 1 list2d |> List.concat |> List.length

        -- 2行目にmarkdownテーブルの左右位置フォーマットを追加
        combined =
            appendTextAlignFormatter list n

        text =
            combined
                |> List.map addVerticalBar
                |> List.map appendLineBreak
                |> String.concat
    in
    text



{-
   markdown用: 先頭と末尾に"|"を追加
   - 空の文字列の場合は何もしない
   - 先頭または末尾にすでに"|"がある場合は何もしない
-}


addVerticalBar : String -> String
addVerticalBar text =
    case String.length text of
        0 ->
            text

        _ ->
            let
                -- 先頭が"|"の場合はtextをそのまま返す
                result1 =
                    if String.startsWith "|" text then
                        text

                    else
                        String.append "|" text

                -- 末尾が"|"の場合はtextをそのまま返す
                result2 =
                    if String.endsWith "|" result1 then
                        result1

                    else
                        String.append result1 "|"
            in
            result2



{-
   markdown用: 配列の末尾にmarkdownのフォーマットを追加
   1. 入力した配列から最初の要素を取得
   2. 2行目に追加するmarkdownフォーマット (:--|:--...)を生成
   3. 1行目と2行目を配列化
   4. 入力配列の2行目移行を抽出
   5. 3と4を組み合わせて出力用の配列を生成
-}


appendTextAlignFormatter : List String -> Int -> List String
appendTextAlignFormatter list n =
    case n of
        0 ->
            list

        _ ->
            let
                first =
                    List.take 1 list

                second =
                    List.repeat n ":--" |> String.join "|"

                firstAndSecond =
                    List.append first [ second ]

                -- 残りの行を抽出
                remain =
                    List.drop 1 list

                combined =
                    firstAndSecond ++ remain
            in
            combined



{-
   文字列の先頭と末尾の"|"を削除し、"|"で配列に分割する
   splitStringByVerticalBar "|column1|column2|"
   -> ["column1", "column2"]
-}


splitStringByVerticalBar : String -> List String
splitStringByVerticalBar text =
    text
        |> dropVerticalBar
        |> String.split "|"



{-
   文字列の先頭または末尾に含まれる"|"を削除する
   dropVerticalBar "|column1|column2|"
   -> "column1|column2"
-}


dropVerticalBar : MarkdownText -> String
dropVerticalBar markdown =
    let
        -- 先頭が"|"の場合削除
        removeStart =
            if String.startsWith "|" markdown then
                String.dropLeft 1 markdown

            else
                markdown

        -- 末尾が"|"の場合削除
        removeEnd =
            if String.endsWith "|" removeStart then
                String.dropRight 1 removeStart

            else
                removeStart

        result =
            removeEnd
    in
    result
