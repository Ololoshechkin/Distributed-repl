# haskell_project

Проект распределенный и состоит из следующих независимых компонент: \

1. Клиент с UI, где есть поле для ввода кода, поле с выводом результата, кнопки compile, open, save и quit: \

![Image 1](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image1.png)
![Image 2](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image2.png)
![Image 3](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image3.png)
 \
Также всплывающие сообщения о различных (сетевых/парсинга/компиляции) ошибках. \

![Image 4](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image4.png)
![Image 5](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image5.png)
![Image 6](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image6.png)
 \
Клиент умеет парсить код на моем языке программирования, сериализовать AST в байты и отправлять мастеру. \

Грамматика поддерживаемого языка: \
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
 \
Note: '::' is an equivalent of integer operator '==' but for Strings \
Note: '##' is an equivalent of integer operator '+' but for Strings \
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

Пример программы: \

`var             i = 0 \
var s = "a" \
var inc  = function(x) { \
	var y = (x) + (1) \
	return y \
} \
var isOdd = function(x) { \
	var y = ((x)  / (2)) * (2) \
	var res = if ((x) == (y)) then (true) else (false) \
	return res \
} \
while ((i) < (5)) { \
         var i = inc(i) \
         var s = if (isOdd(i)) then ((s) ## ("c")) else ((s) ## ("y")) \
} \
PUBLISH (s) ("aaa") 1 \
LOAD (s) q \
return q `

2. Мастер:
Слушает подключения клиентов и подключения новых воркеров (система динамическая, воркеры могут появляться и исчезать). 
- Получает запрос от клиента в виде АСТ программы, шлет одному из доступных воркеров (случайному),
  шлет ему id клиента и программу, забывает про запрос.
- Получает RegisterWorker от нового воркера, добавляет его в список готовых к работе,
- Получает CompilationError + описание + id клиента — шлет клиенту (id) чтоб тот нарисовал окошко ошибки
- Получает Result + результат (строку) + id клиента — шлет клиенту (id) чтобы тот показал результат

Мастер асинхронный и параллельный, не ждет ни чьего ответа, только перегоняет запросики кому надо и забывает,
а также держит мапу соединений и доступных воркеров. Также поддерживается случай, когда воркеров на данный
момент нет, в этом случае клиентский поток будет ждать, когда воркер появится.

3. Воркер:
Подключается к мастеру, шлет RegisterWorker.
Ждет запорсов от мастера, при получении запроса — делает forkIO, исполняет код программы, 
при необходимости общается с БД, 
высылает результат назад мастеру по готовности (ошибка компиляции + описание -- тоже результат).

4. БД. Есть 2 типа запросов: 
publish key value replication.
load key.
БД самописная, устроена так:
4.1 БД-мастер.
При получении запроса publish просто пихает его нужному replication числу БД-воркеров (они выбираются рандомно для redundancy).
При получении load-запроса он пихает всем бд-воркерам и ждет любого первого ответившего.
БД-мастер асинхронный, но имеет таймауты для ответа на клиентские запросы.
4.2 Бд-воркер
Слушает запросы и пишет на диск/читает с диска в файл с названием key пишет value. Последовательное исполнение.

Смысл БД в том, что в коде есть команды load k и publish k v r.
И если несколько клиннтов, то они могут шарить общие переменные или переиспользовать старые.
![Image 7](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image7.png)
