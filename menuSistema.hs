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
            "1" -> menuOperacional
            "2" -> menuGeneral
            "3" -> putStrLn("Nos vemos!")
            _ -> do 
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
            "1" -> menuTrabajadores
            "2" -> opcionesHerramientas
            "3" -> registrarMostrarParcelas
            "4" -> menuInformeCosechas
            "5" -> menuPrincipal
            _ -> do
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
                    mostrarHerramientasUSER "herramientas.json"
                    putStrLn ("Finalizado con éxito...")
                    opcionesHerramientas
            "3" -> menuOperacional
            _ -> do
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
            "3" -> menuOperacional
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
            "6" -> menuOperacional --FALTA AGREGAR LAS FUNCIONALIDADES
            _ -> do
                putStrLn("Error, comando incorrecto")
                menuInformeCosechas


    menuGeneral :: IO()
    menuGeneral = do
        putStrLn("-----Menu general-----")
        putStrLn("1- Gestion de cosechas")
        putStrLn("2- Cierre de cosecha")
        putStrLn("3- Menu consulta de cosecha")
        putStrLn("4- Cancelacion  o modificacion de cosechas")
        putStrLn("5- Consulta disponibilidad de parcela")
        putStrLn("6- Regresar")
        putStrLn("Elige una opción porfavor")
        opcion <- getLine
        case opcion of
            "1" -> do
                obtenerDatosCosecha
                menuGeneral
            "3" -> menuConsultaCosecha
            "6" -> menuPrincipal
            _ -> do
                putStrLn("Error, comando incorrecto")
                menuGeneral
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
                putStrLn("Desarrollando...")
                menuConsultaCosecha
            "3" -> menuGeneral
            _ -> do
                putStrLn("Error, comando incorrecto")
                menuConsultaCosecha
    opcionesCancelacionModificacion :: IO()
    opcionesCancelacionModificacion = do
        putStrLn("1- Modificar cosecha")
        putStrLn("2- Cancelar cosecha")
        putStrLn("3- Regresar")
        putStrLn("Elige una opción porfavor")
    
    opcionesConsultaDisponibilidad :: IO()
    opcionesConsultaDisponibilidad = do
        putStrLn("1- Ver parcelas disponibles en rango de fechas")
        putStrLn("2- Ver disponibilidad por día en un rango de fechas")
        putStrLn("3- Regresar")
        putStrLn("Elige una opción porfavor")