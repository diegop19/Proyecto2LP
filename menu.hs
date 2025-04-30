module MenuPrincipal where

    menuPrincipal ::  IO()
    menuPrincipal = do 
        putStrLn("Bienvenido al menú principal Sistema agricola")
        putStrLn("1- Ver información de trabajadores")
        putStrLn("2- Cargar y mostrar Herramientas de campo")
        putStrLn("3- Registrar y mostrar parcelas de cultivo")
        putStrLn("4- Informe de cosechas")
        putStrLn("5- Salir")
        putStrLn("Elige una opción porfavor")
        opcion <- getLine
        putStrLn ("Elegiste " ++ opcion)
        if opcion == "5" then putStrLn ("Hasta luego!")
        else menuPrincipal
    