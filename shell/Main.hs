-- Сейчас программа должна уметь следующее: считывает комманды с stdin, выполняет их, не падает (!)

module Main where

import Shell

-- Для работы со строкой ввода удобно испольовать библиотеку readline.

-- Функция command cmd args выполняет комманду cmd с аргументами args.
-- Есть несколько встроенных комманд: cd, pwd, ... потом еще добавим может быть.
-- Если это не встроенная команда, то пытаемся запустить внешнее приложение.
command :: String -> [String] -> Shell ()
command = undefined

main = undefined