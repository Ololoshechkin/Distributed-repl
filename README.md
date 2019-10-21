# haskell_project

The project is distributed and is composed by the following components: 

1. Client with UI:
* Code text field
* Program result label
* Buttons: compile, open, repl mode, save и quit: 

![Image 1](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image1.png)
![Image 2](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image2.png)
![Image 3](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image3.png)
 

If any error (network error/parsing error/compilation error) happens there is an error message window for that:


![Image 4](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image4.png)
![Image 5](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image5.png)
![Image 6](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image6.png)


A client parses source code written on a programming language designed by myself, then the client serializes the AST of the code and sends to master.

Also (new feature) there is a Repl Mode that allows client to send code fragments (statements or expressions) to execute/evaluate. In case of statements a Repl state of the client changes remotely:

![Image 8](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image8.png)

![Image 9](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image9.png)

Grammar of my language that I support:

* Program = (Statement*) ReturnStatement
* Statement = (Assignment | Loop | Invocation | DBComand)
* Assignment = 'var' Identifier '=' Expression
* Loop = 'while' BracesExpression '{' (Statement*) '}' 
* Expression = Invocation | Constant | OperatorExpression | IfThenElseExpression | LambdaDefExpression | BracesExpression
* Invocation = Identifier '(' Expression (',' Expression)* ')'
* Identifier = [a-z|A-Z]+ (note : names like "function", "return", "if" or other keywords are not allowed. If you use such names nothing can be guaranteed but in most SANE! cases they do work!)
* Constant = IntConstant | StringConstant | BoolConstant
* IntConstant = [0-9]+ (note: 00091 <=> 91)
* StringConstant = \"[a-z0-9A-Z_]*\"
* BoolConstant = 'true' | 'false' 
* OperatorExpression : UnaryOpExpr | BinaryOpExpr
* UnaryOpExpr = UnaryOp BracesExpression
* BinaryOpExpr = BracesExpression BinaryOp BracesExpression
* UnaryOp : '+' | '-' | '!'
* BinaryOp : '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '&&' | '||' | '::' | '##'

Note: '::' is an equivalent of integer operator '==' but for Strings

Note: '##' is an equivalent of integer operator '+' but for Strings

* IfThenElseExpression = 'if' BracesExpression 'then' BracesExpression 'else' BracesExpression
* LambdaDefExpression = 'function' '(' (Identifier (',' Identifier)*)? ')' '{' Program '}'
* BracesExpression = '(' Expression ')'
* DBComand = PublishComand | LoadComand
* PublishComand = 'PUBLISH' Key Value ReplicationFactor 
* Key = BracesExpression of type String
* Value = BracesExpression of type String
* ReplicationFactor = IntConstant
* LoadComand = 'LOAD' Key Identifier 
* Key = BracesExpression of type String
* ReturnStatement = 'return' Expression

Valid program example:


```
var             i = 0  
var s = "a"  
var inc  = function(x) {  
	var y = (x) + (1)  
	return y   
}   
var isOdd = function(x) { 
	var y = ((x)  / (2)) * (2) 
	var res = if ((x) == (y)) then (true) else (false) 
	return res 
} 
while ((i) < (5)) { 
         var i = inc(i) 
         var s = if (isOdd(i)) then ((s) ## ("c")) else ((s) ## ("y")) 
} 
PUBLISH (s) ("aaa") 1 
LOAD (s) q 
return q 
```

It's result is a database value for the key "aycycy" assigned to "aaa" and program output is "aaa".

2. Master:
Listens for new client and worker connections. Master is fully async, processes requests in parallel and supports dynamic connections/disconnections of workers.
Master reacts on the following requests:
- Receives ClienRequest (AST of a program to compile). Sends WorkerRequest(ClientRequest + ID of the client) to randomly-chosen worker that is currently availabele, then master forgets about the request.
- Receives RegisterWorker from new worker, adds it to the list of available workers.
- Receives CompilationError(description) + ID of a client. Sends CompilationError(description) to the client with ID.
- Receives Result(program output) + ID of the client. Sends Result(program output) to the client with ID.

3. Воркер:
Подключается к мастеру, шлет RegisterWorker.
Ждет запорсов от мастера, при получении запроса — делает fork, исполняет код программы, 
при необходимости общается с БД, 
высылает результат назад мастеру по готовности (ошибка компиляции + описание -- тоже результат).
В случае получении repl-запроса ведет себя аналогично обычной компиляции, но начинает исполнение с готового маппинга переменных (repl state), которое читает из базы данных. Засчет этого можно убивать воркеров, которые обрабатывают repl для какого-то из клиентов, и любой другой воркер продолжит с того же момента (клиент не умрет). После завершения исполнения repl-фрагмента или вычисления его результата - происходит запись в базу данных нового repl state.

4. БД. Есть 4 типа запросов: 
publish key value replication - запись (key, value) в таблицу ключей-значений на n=replication машин
load key - чтение (value) из таблицы ключей-значений
downloadMap cid  - чтение (map) из таблицы переменных для repl-state клиента cid
uploadMap cid map - запись (cid, map) в таблицу переменных для repl-state клиента cid
БД самописная, устроена так:
4.1 БД-мастер.
При получении запроса publish просто пихает его нужному replication числу БД-воркеров (они выбираются рандомно для redundancy).
При получении load-запроса он пихает всем бд-воркерам и ждет любого первого ответившего.
Поведение download и upload запросов аналогично load и publish для replication=3
Также БД-мастер умеет понимать, что появились новые БД-воркеры в процессе работы.
БД-мастер асинхронный, но имеет таймауты для ответа на клиентские запросы.
4.2 Бд-воркер
Имеет две реализации, которые могут работать в одной системе вместе параллельно:
4.2.1 Бд-воркер на файловой системе
Слушает запросы и пишет на диск/читает с диска в файл с названием key пишет value. Последовательное исполнение.
Слушает запросы и пишет на диск/читает с диска в файл с названием "cid.map" пишет map. Замечание: поддерживается запись на диск в том числе лямбд (произвольных программ). Последовательное исполнение.
4.2.2 Бд-воркер на SQlite3
Пишет в таблицы kvmaptable и replvarmap, аналогичен воркеру на ФС.

Смысл БД в том, что в коде есть команды load k и publish k v r.
И если несколько клиннтов, то они могут шарить общие переменные или переиспользовать старые.

Пример "чата" с использованием БД:

![Image 7](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image7.png)

Указания по запуску:
1. Запускаем мастера

аргументы: master host, master port
```
stack run master "127.0.0.1" "8999"
```
2. Запускаем БД-мастера (пункты 1 и 2 можно поменять местами)
	
аргументы: dbmaster host, dbmaster port
```
stack run dbmaster "127.0.0.1" "9997"
```
3. Запускаем Воркеров (сколько угодно)
	
аргументы: master address (выводится мастером при запуске), dbmaster address (выводится БД-мастером при запуске), порт на котором слушать мастера, порт на котором слушать БД-мастера
```
stack run worker "127.0.0.1:8999:0" "127.0.0.1:9997:0" "2998" "1999"
```
4. Запускаем БД-воркеров (сколько угодно)

аргументы: dbmaster address, порт на котором слушать БД-мастера, "fs" или "sql" -- какую технологию использовать для данного воркера
```
stack run dbworker "127.0.0.1:9997:0" "6995" fs
```
5. Запускаем клиента (опять же, сколько угодно)
	
аргументы: master address, порт на котором слушать мастера
```
stack run client "127.0.0.1:8999:0" "7777"
```

Notes (отказоустойчивость):
* Если воркер умирает, система продолжает работать. При наличии другого воркера клиенты продолжают получать результаты компиляции.
* Если умер последний воркер --- система работает, но клиенты получают таймаут и рисуют окошко с сообщением.
* Если умер БД-воркер --- система продолжает работать. Клиенты продолжают работать с базой данных если их файлы были записаны с достаточной репликацией, иначе запросы к БД могут начать отваливаться по таймауту.
* При перезапуска БД-воркера на той же машине файлы на диске сохраняются.
* При падении БД-мастера падают все БД-воркеры. Все подключенные к нему обычные воркеры продолжают отвечать на запросы компиляции, не затрагивающие БД.
* При падении мастера падают все воркеры. Клиент не падает, но перестает отвечать на новые запросы компиляции

Затронутые технологии:
* UI (qtah)
* Приложение распределенное
* Использует сетевое взаимодействие
* Взаимодействие с файловой системой
* База данных (SQLite)

Затронутые конструкции:
* Монады отличные от IO: MonadRandom, ParIO
* Трансформеры монад: ReaderT
* Парсер комбинаторы: Вся грамматика скрипта
* Параллельность и Concurrency: PAR, fork, IVar, MVar, TVar и операции с ними
* Линзы: Template haskell+линзы для Server state.

