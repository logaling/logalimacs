# logalimacsで利用できるコマンド
(logaling-commandのについては、[ここ](/about.html)を参照してください。)

## loga-interactive-command
対話的に、logalimacsを実行します。  
ワンコマンドで、logaling-commandを利用したい時に便利です。  


実行すると、以下の様にミニバッファに利用可能なタスクが表示されると思います。  
    
    types prefix of feature that want you :
    a)dd,c)onfig,d)elete,h)elp,i)mport,l)ookup,n)ew,r)egister,U)nregister,u)pdate,v)ersion,f)ly-mode

上のメッセージで、a)ddから始まる文字が利用可能なタスクです。
もしadd(単語の追加)をしたい場合は、キーボードで_a_をタイプします。  
すると、\*logalimacs\*バッファが開き、へルプメッセージが表示されます。
そのメッセージを参考にミニバッファに内容を打ち込みます。  
(logaling-commandをラップしているものは、全て処理前にヘルプメッセージが表示されます。)

この例で打ち込む内容は、loga-add-wordの項を参照してください。
他のタスクも頭文字を指定して同様に、選択可能です。

## loga-add-word
ターミナルからの "% loga add source target note"をラップするコマンドです。  
ターミナル上では、英語で複数の単語をスペース区切りで登録するのに、  
クオートする必要がありますがlogalimacsでこのコマンドを利用する場合は、  
必要ありません。
コマンド実行後、source(元の単語)、target(翻訳後の単語)、注釈を
個別に聞いてきます。  

## loga-update
ターミナルからの "% loga update source target(old) target(new) note"をラップするコマンドです。  
ターミナル上では、英語で複数の単語をスペース区切りで登録するのに、  
クオートする必要がありますがlogalimacsでこのコマンドを利用する場合は、  
必要ありません。
コマンド実行後、source(元の単語)、target(変更前の単語)、target(変更後の単語)、注釈を個別に聞いてきます。  

## loga-lookup-in-hand-or-region
ターミナルからの "% loga lookup target"をラップするコマンドです。
logalimacs独自の機能として、リージョンを選択していた場合、
その単語を、検索します。
それ以外の場合、ミニバッファに検索する文字を入力し、RETする事で利用できます。

## loga-fly-mode
logalimacs独自の機能で、カーソル位置にある単語を"loga lookup"(検索)します。
(空白では、その位置より左側の単語になります。)
この機能は、通常はoffで_loga-interactive-command_から実行するか、
_M-x loga-fly-mode_又は、任意のキーバインドで実行する事で、
onとoffをトグルできます。

## loga-get-flymake-error
flymakeでのエラーをlogalimacsバッファに出力します。
