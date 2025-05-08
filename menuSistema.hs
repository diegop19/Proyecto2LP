module MenuSistema where
    import ManipulacionDatos
    {-------------------------------------------------------------------}
        {-Menu principal del sistema agricola-}
    menuPrincipal :: IO()
    menuPrincipal = do
        putStrLn("Bienvenido al menú principal Sistema agricola")
        putStrLn("1- Menu operacional")
        putStrLn("2- Menu general")
        putStrLn("3- Salir")
        putStrLn("Elige una opción porfavor")
        opcion <- getLine
        case opcion of
            "1" -> do
                clearConsole 
                menuOperacional
            "2" -> do
                clearConsole 
                menuGeneral
            "3" -> do
                clearConsole
                putStrLn("Nos vemos!")
            _ -> do 
                clearConsole
                putStrLn ("Error, comando erroneo")
                menuPrincipal

{-------------------------------------------------------------------}    
    -- menu operacional del sistema agricola
    menuOperacional ::  IO()
    menuOperacional = do 
        putStrLn("-------Menu Operacional------")
        putStrLn("1- Opciones de trabajadores")
        putStrLn("2- Cargar y mostrar Herramientas de campo")
        putStrLn("3- Registrar y mostrar parcelas de cultivo")
        putStrLn("4- Informe de cosechas")
        putStrLn("5- Regresar")
        putStrLn("Elige una opción porfavor")
        opcion <- getLine
        case opcion of
            "1" -> do
                clearConsole 
                menuTrabajadores
            "2" -> do
                clearConsole 
                opcionesHerramientas
            "3" -> do
                clearConsole
                registrarMostrarParcelas
            "4" -> do
                clearConsole
                menuInformeCosechas
            "5" -> do 
                menuPrincipal
            _ -> do
                clearConsole
                putStrLn("Error, comando incorrecto")
                menuOperacional
{-----------------------------------------------------------}
-- MENU DE TRABAJADORES
    menuTrabajadores :: IO()
    menuTrabajadores = do
        putStrLn("---------Menu Trabajadores-------")
        putStrLn("1 - Agregar Trabajador")
        putStrLn("2 - Ver trabajadores")
        putStrLn("3 - Regresar")
        putStrLn("Elija una opción porfavor")
        opcion <- getLine
        case opcion of
            "1" -> do 
                agregarTrabajador
                menuTrabajadores
            "2" -> do
                mostrarListaTrabajadoresUSER "trabajadores.json"
                menuTrabajadores
            "3" -> menuOperacional
            _ -> do
                clearConsole
                putStrLn("Error, comando incorrecto")
                menuTrabajadores

    {------------------------------------------------------------------------}
    -- MOSTRAR OPCIONES DE HERRAMIENTAS
    opcionesHerramientas :: IO()
    opcionesHerramientas = do
        putStrLn("1- Cargar herramientas desde archivo")
        putStrLn("2- Ver herramientas")
        putStrLn("3- Volver atras")
        putStrLn("Por favor elige una opcion")
        opcion <- getLine
        case opcion of
            "1" -> do
                    putStrLn("Ingrese la ruta de las herramientas.")
                    rutaHerramientas <- getLine
                    guardarHerramientasUSER "herramientas.json" rutaHerramientas
                    opcionesHerramientas

            "2" -> do
                    clearConsole
                    mostrarHerramientasUSER "herramientas.json"
                    putStrLn ("Finalizado con éxito...")
                    opcionesHerramientas
            "3" -> do
                clearConsole
                menuOperacional
            _ -> do
                clearConsole
                putStrLn("Error comando incorrecto")
                opcionesHerramientas
{-----------------------------------------------------------------}
    --MOSTRAR OPCIONES DE MOSTRAR PARCELA Y REGISTRAR PARCELA
    registrarMostrarParcelas :: IO()
    registrarMostrarParcelas = do
        putStrLn("1- Mostrar parcelas existentes")
        putStrLn("2- Registrar parcela")
        putStrLn("3- Regresar")
        putStrLn("Elige una opción porfavor")
        opcion <- getLine
        case opcion of
            "1" -> do
                mostrarParcelasUSER 
                registrarMostrarParcelas
            "2" -> do
                agregarParcela
                registrarMostrarParcelas
            "3" -> do
                clearConsole 
                menuOperacional
            _ -> do
                putStrLn("Error, comando incorrecto")
                registrarMostrarParcelas
{-------------------------------------------------------------------}
    --MENU INFORME DE COSECHAS
    menuInformeCosechas :: IO()
    menuInformeCosechas = do
        putStrLn("1- Parcela con mayor volumen de cosecha")
        putStrLn("2- Top 3 parcelas con mayor venta")
        putStrLn("3- Trabajador con más cosechas realizadas")
        putStrLn("4- Mes - Año con mayor recoleccion acumulada")
        putStrLn("5- Cosechas con sub/sobre produccion")
        putStrLn("6- Regresar")
        putStrLn("Elige una opción porfavor")
        opcion <- getLine
        case opcion of
            "1" -> do
                putStrLn ("---------PARCELA CON MAYOR VOLUMEN DE COSECHA--------")
                topParcelaMayorVolumen
                menuInformeCosechas
            "2" -> do
                putStrLn("----------TOP 3 PARCELAS CON MAYOR VENTA----------------")
                topTresParcelasVentas
                menuInformeCosechas
            "3" -> do
                putStrLn("----------TRABAJADOR CON MÁS COSECHAS---------------")
                mostrarTrabajadorConMasCosechas
                menuInformeCosechas
            "5" -> do
                putStrLn("-------COSECHAS CON SOBREPRODUCCION---------\n")
                cosechasSobreproduccion
                putStrLn("\n-------COSECHAS CON SUBPRODUCCION------------\n")
                cosechasSubproduccion
                menuInformeCosechas

            "6" -> menuOperacional --FALTA AGREGAR LAS FUNCIONALIDADES
            _ -> do
                putStrLn("Error, comando incorrecto")
                menuInformeCosechas

    menuGeneral :: IO()
    menuGeneral = do
        putStrLn "\n----- MENÚ GENERAL -----"
        putStrLn "1- Gestión de cosechas"
        putStrLn "2- Cierre de cosecha"
        putStrLn "3- Consulta de cosecha"
        putStrLn "4- Cancelación o modificación de cosechas"
        putStrLn "5- Consulta disponibilidad de parcela"
        putStrLn "6- Regresar al menú principal"
        putStr "Elige una opción: "
        opcion <- getLine
        
        case opcion of
            "1" -> do
                clearConsole
                obtenerDatosCosecha
                menuGeneral                
            "2" -> do
                clearConsole
                pedirCodigoCosecha
                menuGeneral
            "3" -> do
                clearConsole
                menuConsultaCosecha
            "4" -> do
                clearConsole
                opcionesCancelacionModificacion
            "5" -> do
                clearConsole
                menuConsultaDisponibilidad
                
            "6" -> do
                clearConsole
                menuPrincipal
                
            _ -> do
                clearConsole
                putStrLn "Error: Opción no válida"
                menuGeneral

    menuConsultaDisponibilidad :: IO()
    menuConsultaDisponibilidad = do
        putStrLn "\n ------ Consulta de Disponibilidad ------"
        putStrLn "1- Ver parcelas disponibles en un rango de fechas"
        putStrLn "2- Ver disponibilidad diaria por parcela"
        putStrLn "3- Volver al menú general"
        putStr "Seleccione una opción: "
        opcion <- getLine
        
        case opcion of
            "1" -> do
                clearConsole
                putStrLn "\nIngrese fecha de inicio (AAAA-MM-DD):"
                inicio <- pedirFechaValida
                putStrLn "Ingrese fecha de fin (AAAA-MM-DD):"
                fin <- pedirFechaValida
                consultarDisponibilidadBasica inicio fin
                menuConsultaDisponibilidad
                
            "2" -> do
                clearConsole
                putStrLn "\nIngrese fecha de inicio (AAAA-MM-DD):"
                inicio <- pedirFechaValida
                putStrLn "Ingrese fecha de fin (AAAA-MM-DD):"
                fin <- pedirFechaValida
                consultarDisponibilidadDetallada inicio fin
                menuConsultaDisponibilidad
                
            "3" -> do
                clearConsole
                menuGeneral
                
            _ -> do
                clearConsole
                putStrLn "Error: Opción no válida"
                menuConsultaDisponibilidad

    menuConsultaCosecha :: IO()
    menuConsultaCosecha = do
        putStrLn("------Menu Consulta Cosecha------")
        putStrLn("1 - Mostrar todas las cosechas")
        putStrLn("2 - Buscar cosecha por codigo de Cosecha")
        putStrLn("3 - Regresar")
        putStrLn("Elige una opcion")
        opcion <- getLine
        case opcion of
            "1" -> do
                mostrarCosechasUSER
                menuGeneral
            "2" -> do
                putStrLn("Ingrese el número de cosecha que quiere imprimir")
                numCosecha <- getLine
                mostrarCosechaSolaUSER numCosecha
                menuConsultaCosecha
            "3" -> menuGeneral
            _ -> do
                putStrLn("Error, comando incorrecto")
                menuConsultaCosecha

    opcionesCancelacionModificacion :: IO()
    opcionesCancelacionModificacion = do
        putStrLn("--------- Cancelar/Modificar Cosecha --------")
        putStrLn("1- Modificar cosecha")
        putStrLn("2- Cancelar cosecha")
        putStrLn("3- Regresar")
        putStrLn("Elige una opción por favor")
        opcion <- getLine
        case opcion of
            "1" -> do
                clearConsole
                modificarCosecha
                opcionesCancelacionModificacion
            "2" -> do
                clearConsole
                mostrarCosechasUSER  
                cancelarCosechaUSER
                opcionesCancelacionModificacion
            "3" -> menuGeneral
            _ -> do
                putStrLn "Opción inválida"
                opcionesCancelacionModificacion
    
    opcionesConsultaDisponibilidad :: IO()
    opcionesConsultaDisponibilidad = do
        putStrLn("1- Ver parcelas disponibles en rango de fechas")
        putStrLn("2- Ver disponibilidad por día en un rango de fechas")
        putStrLn("3- Regresar")
        putStrLn("Elige una opción porfavor")