{-# LANGUAGE DeriveGeneric #-}
module ManipulacionDatos (mostrarHerramientasUSER, guardarHerramientasUSER, mostrarListaTrabajadoresUSER, agregarParcela, mostrarParcelasUSER, obtenerDatosCosecha, agregarTrabajador, mostrarCosechasUSER, mostrarCosechaSolaUSER,pedirCodigoCosecha,cancelarCosechaUSER ,modificarCosecha,modificarFechasCosecha,modificarParcelaCosecha,modificarVegetalCosecha,consultarDisponibilidadBasica,consultarDisponibilidadDetallada,pedirFechaValida,clearConsole,topTresParcelasVentas,topParcelaMayorVolumen, cosechasSobreproduccion,cosechasSubproduccion,mostrarTrabajadorConMasCosechas) where
    import System.Process
    import Text.Printf (printf)
    import Text.Read(readMaybe)
    import GHC.Generics (Generic)
    import System.Directory (doesFileExist)
    import Data.Time(Day, formatTime, defaultTimeLocale)
    import Data.Time.Clock(getCurrentTime, utctDay)
    import Data.Time.Format (parseTimeM, defaultTimeLocale)
    import Data.Aeson 
    import Data.Ord (comparing)
    import Data.Maybe (catMaybes)
    import Data.List.Split (splitOn)
    import Data.List (maximumBy, sortBy)
    import Data.List (nubBy)
    import Data.List (find)
    import Data.Time (Day, formatTime, defaultTimeLocale, addDays)  -- Añadir addDays aquí
    import Data.Time.Clock (getCurrentTime, utctDay)
    import Data.Time.Format (parseTimeM, defaultTimeLocale)
    import qualified Data.ByteString.Lazy.Char8 as BL
    --Data herramienta que funciona para guardar en archivos .json

    data Herramienta = Herramienta {   
          codigoHerramienta :: String
        , nombreHerramienta :: String
        , descripcionHerramienta :: String
        , tipoHerramienta :: String
    } deriving (Show, Generic)

    instance FromJSON Herramienta
    instance ToJSON Herramienta
-- Data trabajador que nos sirve para guardar trabajadores en archivo .json
    data Trabajador = Trabajador {
          cedula :: String
        , nombreCompleto :: String
        , rol :: String
        , cantidadCosechasTrabajadas :: Int
    }deriving (Show, Generic)

    instance FromJSON Trabajador
    instance ToJSON Trabajador
-- Data vegetal para guardar tipos de vegetales
    data Vegetal = Vegetal {
          tipoVegetal :: String
        , precioPorKilo :: Double
    }deriving (Show, Generic)
    instance FromJSON Vegetal
    instance ToJSON Vegetal
-- Dta parcela que funciona para guardar parcelas en un archivo.json
    data Parcela = Parcela {
          codigoParcela :: Int
        , nombreParcela :: String
        , zona :: String
        , areaEnMetrosCuadrados :: Int
        , vegetales :: [Vegetal] --Guarda una lista de todos los vegetales de la parcela
        , herramientas :: [Herramienta] -- Guarda las herramientas asociadas a la parcela
        , historialVenta :: Double
        , volumenCosecha :: Double
    }deriving (Show, Generic)

    instance FromJSON Parcela
    instance ToJSON Parcela
-- Data cosecha para guardar los datos en un .json
    data Cosecha = Cosecha {
          identificadorCosecha :: Int
        , idParcelaCosecha :: Int
        , trabajadores :: [Trabajador] -- guarda los trabajadores asociados a esa cosecha
        , fechaInicio :: Day
        , fechaFinal :: Day
        , produccionEsperada :: Double
        , produccionObtenida :: Double
        , tipoVegetalCosecha :: String
        , estadoCosecha :: Bool

    }deriving(Show, Generic)

    instance FromJSON Cosecha
    instance ToJSON Cosecha
-- función que nos ayuda a limpiar la consola para una visualización más limpia
    clearConsole :: IO ()
    clearConsole = do
        _ <- system "cls"  -- Ejecuta el comando 'cls' en la consola de Windows
        return ()

--TODA ESTA ZONA ES DE TRABAJADORES Y HERRAMIENTAS
{----------------------------------------------------------------------------}
    -- funcion que recibe una ruta donde están los trabajdores y retorna una lista de trabajadores 
    obtenerTrabajadores :: FilePath -> IO[Trabajador]
    obtenerTrabajadores archivoTrabajadores = do
        contenido <-  BL.readFile archivoTrabajadores
        case eitherDecode contenido of
            Right hs-> return hs -- Si encuentra la lista la retorna
            Left _-> return [] -- si no retorna la lista vacía
    -- recibe una lista de trabajadores e imprime los elementos een esa lista
    mostrarListaTrabajadores :: [Trabajador] -> IO ()
    mostrarListaTrabajadores trabajadores = do
        putStrLn encabezado
        putStrLn lineaSeparadora
        mapM_ imprimirFila trabajadores
        where --Definimos las cosas que necesitaremos para la impresión
            encabezado = printf "%-10s | %-35s | %-10s | %-20s"
                                "Cedula" "Nombre" "rol" "Cosechas"
            lineaSeparadora = replicate 75 '-'
            imprimirFila (Trabajador ced nom rol cosechas) =
                printf "%-10s | %-35s | %-10s | %-20s\n" ced nom rol (show cosechas) -- usamos show para imprimir las cosechas que ha trabajado
    -- creamos un archivo .json con un nombre que se le pase    
    crearArchivoJSON :: String -> IO()
    crearArchivoJSON nombreArchivo = do
        BL.writeFile nombreArchivo (BL.pack "[]") -- Lo creamos con una lista
        putStrLn $ "Archivo "++ nombreArchivo ++" Creado exitosamente."
    
    -- funcion que verifica si un archivo existe, nos ayuda principalmente para saber si hay que crearlo
    validarExistencia :: String -> IO Bool
    validarExistencia nombreArchivo = doesFileExist nombreArchivo --Renorna true si encuentra el archivo

    -- guardamos los trabajadores en una lista que se le pase de trabajadores y las guarda en la ruta indicada
    guardarTrabajadores :: [Trabajador] -> FilePath -> IO()
    guardarTrabajadores ts archivo = do
        existe <- validarExistencia archivo
        if existe then
            BL.writeFile archivo (encode ts)
            else do
                crearArchivoJSON archivo
                BL.writeFile archivo (encode ts)

    -- Función que nos ayuda a agregar un trabajador, se le piden los datos al usuario
    agregarTrabajador :: IO()
    agregarTrabajador = do
        putStrLn("Agregue el numero de cédula del trabajador")
        cedula <- getLine
        putStrLn("Agregue el nombre del trabajador")
        nombre <- getLine
        putStrLn("Agregue el rol del trabajador")
        rol <- getLine
        existe <- validarExistencia "trabajadores.json" --se valida si existe el archivo al que se quiere agregar
        if existe then putStrLn("Archivo encontrado") -- si existe entonces imprimimos un mensaje
        else crearArchivoJSON "trabajadores.json" --si no existe lo creamos
        ts <- obtenerTrabajadores "trabajadores.json" -- obtenemos los trabajadores que existen en la lista
        let cosechasTrabajadas = 0 
        let nuevoTrabajador = Trabajador cedula nombre rol cosechasTrabajadas --creamos el nuevo dato trabajadoor
        let listaTrabajadores = ts ++ [nuevoTrabajador] --agregamos a la lista obtenida
        guardarTrabajadores listaTrabajadores "trabajadores.json" -- guardamos de nuevo en el archivo

    -- función que nos retorna las herramientas del archivo de herramientas
    obtenerHerramientas :: FilePath -> IO [Herramienta]
    obtenerHerramientas archivo = do
        existe <- validarExistencia archivo  
        if existe
            then do
                contenido <- BL.readFile archivo
                case eitherDecode contenido of
                    Right hs -> return hs
                    Left _ -> return []
            else do
                crearArchivoJSON "herramientas.json"
                return []
    -- función que recibe una lista de herramientas e imprime todas las herramientas en esa lista
    mostrarListaHerramientas :: [Herramienta] -> IO ()
    mostrarListaHerramientas herramientas = do
        putStrLn encabezado
        putStrLn lineaSeparadora
        mapM_ imprimirFila herramientas
        where
            encabezado = printf "%-10s | %-15s | %-30s | %-10s"
                            "Código" "Nombre" "Descripción" "Tipo"
            lineaSeparadora = replicate 75 '-'
            imprimirFila (Herramienta cod nom desc tipo) = --Definimos imprimirFila para poder imprimir uno a uno con map las herramientas de la lista
                printf "%-10s | %-15s | %-30s | %-10s\n" cod nom desc tipo

    -- convertimos todas las lineas del archivo plano e una herramienta usando maybe
    convertirHerramienta :: String -> Maybe Herramienta
    convertirHerramienta herramienta = 
        case splitOn "," herramienta of
            [codigo, nombre, descripcion, tipo] -> Just (Herramienta codigo nombre descripcion tipo) -- si la linea tiene lo que se espera se convierte en una herramienta
            _ ->
                Nothing --si una linea no tiene lo que se espera se omite
    -- Recibe una ruta donde estan las herramientas que se quieren agregar
    leerArchivoPlano :: FilePath -> IO[Herramienta] --devuelve una lista de herramientas
    leerArchivoPlano ruta = do
        contenido <- readFile ruta -- lee el contenido del archivo
        let lineas = lines contenido -- separa el contenido en elementos de una lista usando los saltos de lineas como separadores
            herramientas = mapM convertirHerramienta lineas --pasa cada linea para convertirla en una herramienta
        case herramientas of -- se valora lo que sucede
            Just hs -> do
                putStrLn("Archivo leido exitosamente") 
                putStrLn ("Guardando las herramientas...") 
                return hs -- si todo estaba bien se retorna la lista
            Nothing -> do
                putStrLn("Error, el archivo tiene alguna(s) lineas mal")
                return [] -- si no se retorna una lista vacia
    -- Recibe una ruta donde guardar la lista de herramientas
    guardarHerramientasJSON :: FilePath -> [Herramienta] -> IO()
    guardarHerramientasJSON rutaJSON herramientasLeidas = do
        herramientasExistentes <- obtenerHerramientas rutaJSON -- obtiene las herramientas del archivo .json
        let todasHerramientas = herramientasExistentes ++ herramientasLeidas --une las nuevas herramientas y las herramientas existentes
            herramientasActualizadas = eliminarDuplicados todasHerramientas -- eliminamos las herramientas duplicadas en la lista unida
        mostrarListaHerramientas herramientasActualizadas  -- mostramos las herramientas 
        BL.writeFile rutaJSON(encode herramientasActualizadas) -- guardamos las herramientas
        putStrLn ("Herramientas guardadas exitósamente...")
    -- función para eliminar herramientas duplicadas
    eliminarDuplicados :: [Herramienta] -> [Herramienta]
    eliminarDuplicados = nubBy (\h1 h2 -> codigoHerramienta h1 == codigoHerramienta h2)

    -- función que es para guardar herramientas pero sólo visible para el modulo de menus
    guardarHerramientasUSER :: FilePath -> FilePath -> IO()
    guardarHerramientasUSER archivoJSON archivoHerramientas = do 
        herramientasNuevas <- leerArchivoPlano archivoHerramientas
        guardarHerramientasJSON archivoJSON herramientasNuevas
        putStrLn("Éxito.")
    -- función para mostrar las herramientas pero sólo visible para el modulo de menus
    mostrarHerramientasUSER :: FilePath -> IO()
    mostrarHerramientasUSER rutaJSON = do
        listaHerramientas <- obtenerHerramientas rutaJSON
        mostrarListaHerramientas listaHerramientas

    -- funcion para mostrar trabajadores pero sólo visible para el modulo de menus
    mostrarListaTrabajadoresUSER :: FilePath -> IO()
    mostrarListaTrabajadoresUSER archivoTrabajadores = do
        existe <- validarExistencia archivoTrabajadores
        if existe then putStrLn("Archivo encontrado")
        else crearArchivoJSON archivoTrabajadores

        listaTrabajadores <- obtenerTrabajadores archivoTrabajadores
        mostrarListaTrabajadores listaTrabajadores  
{------------------------------------------------------------------------------}
--DE AQUÍ EN ADELANTE TRABAJAREMOS CON LA MANIPULACIÓN DE PARCELAS
    -- funcion que recibe el codigo de la herramienta y la lista de herramientas y si encuentra coincidencia
    buscarHerramienta :: String -> [Herramienta] -> Maybe Herramienta-- retorna la herramienta
    buscarHerramienta codigo hs = find (\h -> codigoHerramienta h == codigo) hs
    -- recibe un string de codigos de herramienta separados por comas y la lista de herramientas 
    obtenerHerramientasParaParcela :: String -> [Herramienta] -> [Herramienta]
    obtenerHerramientasParaParcela codigos listaHerramientas = 
        let codigosSeparados = splitOn "," codigos -- separa los codigos en una lista usando la coma como separador
            herramientasEncontradas = map (`buscarHerramienta` listaHerramientas) codigosSeparados -- tomamos las herramientas con los códigos de las herramientas encontrados
        in catMaybes herramientasEncontradas
    -- función para pedir el precio de un vegetal recursivamente hasta que se ingrese un formato válido
    pedirPrecio :: IO Double
    pedirPrecio = do
        putStrLn "Escriba el precio por Kg."
        input <- getLine
        case readMaybe input of
            Just precio -> return precio -- si tiene formmato de double se retorna
            Nothing -> do
                putStrLn "Entrada inválida. Intenta de nuevo." -- si no se pide de nuevo
                pedirPrecio

    -- Función para pedir el area de la parcela hasta que se ingrese un entero válido
    pedirArea :: IO Int
    pedirArea = do
        putStrLn "Escriba el area en mts**2"
        input <- getLine
        case readMaybe input of
            Just area -> return area -- si es valido retorna el area
            Nothing -> do
                putStrLn "Entrada inválida. Intenta de nuevo." -- si no es valido se pide que se ingrese de nuevo
                pedirArea
    -- lee las parcelas que estan en el .json y retorna una lista de parcelas
    leerParcelas :: FilePath -> IO [Parcela]
    leerParcelas archivo = do
        existe <- validarExistencia archivo -- se obtiene si el archivo existe
        if not existe 
            then crearArchivoJSON archivo -- en caso de no existir se crea el archivo
            else putStrLn("")
        contenido <- BL.readFile archivo -- se obtienen los datos del archivo
        case decode contenido of
            Just parcelas -> return parcelas -- si todo sale bien se retorna la lista de parcelas
            Nothing -> return [] -- sino se retorna una lista vacía
    -- esta función retorna el id de la parcela agregada de última y le suma uno para retornar un nuevo id consecutivo
    obtenerNuevoIdParcela :: FilePath -> IO Int
    obtenerNuevoIdParcela archivo = do
        parcelas <- leerParcelas archivo
        return $ case parcelas of
            [] -> 1 -- si la lista está vacía retorna 1 porque será el primier id
            ps -> maximum (map codigoParcela ps) + 1
    -- función para guardar todas las parcelas en la lista en la ruta indicada  
    guardarParcelas :: FilePath -> [Parcela] -> IO ()
    guardarParcelas archivo parcelas = do
        existe <- validarExistencia archivo
        if existe then
            BL.writeFile archivo (encode parcelas)
            else do
                crearArchivoJSON archivo
                BL.writeFile archivo (encode parcelas)
    -- creamos una parcela, recibimos todos los datos que necesitamos del usuario
    crearParcela :: String -> String -> [Vegetal] -> [Herramienta] -> Int -> IO ()
    crearParcela nombreParcela zonaParcela listaVegetales listaHerramientas areaEnMetrosCuadrados = do
        existe <- validarExistencia "parcelas.json"
        if not existe
            then crearArchivoJSON "parcelas.json"
            else putStrLn "Existente"

        listaParcelas <- leerParcelas "parcelas.json"
        id <- obtenerNuevoIdParcela "parcelas.json" -- obtenemos el id siguiente
        let nuevaParcela = Parcela {
            codigoParcela = id,
            nombreParcela = nombreParcela,
            zona = zonaParcela,
            areaEnMetrosCuadrados = areaEnMetrosCuadrados,
            vegetales = listaVegetales,
            herramientas = listaHerramientas,
            historialVenta = 0,
            volumenCosecha = 0
        } -- creamos la parcela
        let parcelasActualizadas = listaParcelas ++ [nuevaParcela] --agregamos la nueva parcela a la lista de parcelas
        guardarParcelas "parcelas.json" parcelasActualizadas -- guardamos la lista actualizada con la nueva parcela
        putStrLn("Parcela guardada exitosamente")
    -- función que nos ayuda a guardar todos los vegetales que existen en una parcela
    pedirVegetales :: [Vegetal] -> IO [Vegetal]
    pedirVegetales acumulados = do
        putStrLn "Ingrese el nombre del vegetal (o escriba 'fin' para terminar):"
        nombre <- getLine
        if nombre == "fin" -- Hasta que el usuario ingrese fin seguimos pidiendole vegetales
            then return acumulados
            else do
                putStrLn "Ingrese el precio por kilo del vegetal:"
                precioStr <- getLine
                let precio = read precioStr :: Double
                let vegetal = Vegetal nombre precio
                pedirVegetales (acumulados ++ [vegetal]) --agregamos vegetales recursivamente
    -- función para pedirle los nuevos datos al usuario de la parcela que va a agregar
    agregarParcela :: IO()
    agregarParcela = do
        putStrLn("Escriba el nombre de la parcela.")
        nombre <- getLine
        putStrLn("Escriba la zona de la parcela.")
        zona <- getLine
        area <- pedirArea

        putStrLn("Ingrese los vegetales de la parcela: ")
        vegetales <- pedirVegetales [] -- pedimos todos los vegetales que se podrán cosechar

        mostrarHerramientasUSER "herramientas.json" -- mostramos las herramientas existentes para que el usuario elija cuales asocia a esta parcela
        putStrLn("Ingrese el código de las herramientas que quiere agregar separados por comas. \n (Los datos erróneos no serán agregados)") 
        listaCodigos <- getLine -- guardamos un string de codigos de herramientas separados por comas
        listaHerramientas <- obtenerHerramientas "herramientas.json"
        let herramientasParcela = obtenerHerramientasParaParcela listaCodigos listaHerramientas -- guardamos las herramientas que coincidan con los codigos
        nuevoId <- obtenerNuevoIdParcela "parcelas.json"
        if null herramientasParcela then do -- si la lista es vacia se le pide al usuario que ingrese al menos una herramienta
            putStrLn("Debes agregar al menos una herramienta para la parcela")
            agregarParcela
        else
            crearParcela nombre zona vegetales herramientasParcela area -- una vez con todos los datos creamos la nueva parcela y la guardamos
    -- funcion que es visible para el modulo de menus
    mostrarParcelasUSER :: IO()
    mostrarParcelasUSER = do
        parcelas <- leerParcelas "parcelas.json"
        mostrarParcelas parcelas

    -- mostramos las parcelas 
    mostrarParcelas :: [Parcela] -> IO ()
    mostrarParcelas parcelas = do
        if null parcelas
        then putStrLn "No hay parcelas registradas." -- si no hay imprimimos que no hay
        else mapM_ imprimirParcela parcelas -- si hay imprimimos todas las parcelas en la lista
    -- función para imprimir un tipo de vegetal
    imprimirVegetal :: Vegetal -> IO ()
    imprimirVegetal (Vegetal tipo precio) = do
        putStrLn $ "  Tipo: " ++ tipo ++ ", Precio por Kg: " ++ show precio
    -- función para imprimir una parcela individual
    imprimirParcela :: Parcela -> IO ()
    imprimirParcela (Parcela id nombre zona area vegetales herramientas totalVenta totalCosechado) = do
        putStrLn $ "ID DE LA PARCELA " ++ show id
        putStrLn $ "Nombre de la parcela: " ++ nombre
        putStrLn $ "Zona: " ++ zona
        putStrLn $ "Área (m²): " ++ show area
        putStrLn "Vegetales cultivados:"
        if null vegetales
            then putStrLn "  No hay vegetales asociados."
            else mapM_ imprimirVegetal vegetales -- imprimimos todos los vegetales que se pueden cultivar
        printf "Total recaudado (en colones): %.2f\n" totalVenta
        printf "Total cosechado (en Kg): %.2f\n" totalCosechado
        putStrLn "Herramientas asociadas:"
        if null herramientas
        then putStrLn "  No hay herramientas asociadas."
        else mapM_ imprimirHerramienta herramientas -- se imprimen todas las herramientas asociadas a la parcela
        putStrLn $ replicate 40 '-'  -- Separador entre parcelas
    -- función para imprimir una herramienta
    imprimirHerramienta :: Herramienta -> IO ()
    imprimirHerramienta (Herramienta codigo nombre descripcion tipo) = do
        printf "  Código: %s, Nombre: %s, Descripción: %s, Tipo: %s\n" codigo nombre descripcion tipo

{-------------------------------------------------------------------------------------------}
--- Consulta de disponiilidad de parcelas 

    choqueFechas :: Day -> Day -> Day -> Day -> Bool
    choqueFechas inicio1 fin1 inicio2 fin2 =
        not (fin1 < inicio2 || inicio1 > fin2)

    parcelaDisponibleEnRango :: Int -> Day -> Day -> [Cosecha] -> Bool
    parcelaDisponibleEnRango idParcela inicio fin cosechas =
        all (\c -> not (choqueFechas inicio fin (fechaInicio c) (fechaFinal c))) $
        filter (\c -> idParcelaCosecha c == idParcela && estadoCosecha c) cosechas

    generarRangoDias :: Day -> Day -> [Day]
    generarRangoDias inicio fin 
        | inicio > fin = []
        | otherwise = inicio : generarRangoDias (addDays 1 inicio) fin

    consultarDisponibilidadBasica :: Day -> Day -> IO ()
    consultarDisponibilidadBasica inicio fin = do
        parcelas <- leerParcelas "parcelas.json"
        cosechas <- obtenerCosechas "cosechas.json"
        
        let parcelasDisponibles = filter (\p -> 
                parcelaDisponibleEnRango (codigoParcela p) inicio fin cosechas) parcelas
        
        putStrLn "\n=== PARCELAS DISPONIBLES ==="
        putStrLn $ "Desde: " ++ show inicio ++ " Hasta: " ++ show fin
        
        if null parcelasDisponibles
            then putStrLn "No hay parcelas disponibles en este rango."
            else mapM_ mostrarInfoParcelaSimple parcelasDisponibles
        where
            mostrarInfoParcelaSimple p = 
                putStrLn $ "ID: " ++ show (codigoParcela p) ++ 
                        " - Nombre: " ++ nombreParcela p ++ 
                        " - Zona: " ++ zona p

    parcelaDisponibleEnDia :: Int -> Day -> [Cosecha] -> Bool
    parcelaDisponibleEnDia idParcela dia cosechas =
        not (any (\c -> idParcelaCosecha c == idParcela && 
                        estadoCosecha c &&
                        dia >= fechaInicio c && 
                        dia <= fechaFinal c) cosechas)

    consultarDisponibilidadDetallada :: Day -> Day -> IO ()
    consultarDisponibilidadDetallada inicio fin = do
        parcelas <- leerParcelas "parcelas.json"
        cosechas <- obtenerCosechas "cosechas.json"
        let rangoDias = generarRangoDias inicio fin
        
        putStrLn "\n=== DISPONIBILIDAD DETALLADA ==="
        putStrLn $ "Rango: " ++ show inicio ++ " a " ++ show fin
        
        mapM_ (mostrarDisponibilidadParcela cosechas rangoDias) parcelas
        where
            mostrarDisponibilidadParcela cosechas dias parcela = do
                putStrLn $ "\nParcela ID: " ++ show (codigoParcela parcela) ++ 
                        " - Nombre: " ++ nombreParcela parcela
                putStrLn $ replicate 40 '-'
                
                mapM_ (mostrarEstadoDia (codigoParcela parcela) cosechas) dias
            
            mostrarEstadoDia idParcela cosechas dia = do
                let disponible = parcelaDisponibleEnDia idParcela dia cosechas
                putStrLn $ show dia ++ ": " ++ 
                        if disponible then "Disponible" else "Ocupada"

{-------------------------------------------------------------------------------------------}
--De aquí en adelante se trabajarán las cosechas
    -- función que nos ayuda a saber si una fecha ingresada cumple con el formato requerido
    validarFormatoFecha :: String -> Maybe Day
    validarFormatoFecha fechaStr =
        parseTimeM True defaultTimeLocale "%Y-%m-%d" fechaStr
    -- función que sigue pidiendo una fecha hasta que se agregue con el formato deseado
    pedirFechaValida :: IO Day
    pedirFechaValida = do
        input <- getLine
        case validarFormatoFecha input of
            Just fecha -> return fecha -- si se agrega correctamente retornamos la fecha
            Nothing -> do
                putStrLn "Formato inválido. Intente de nuevo (ejemplo: 2025-05-01)."
                pedirFechaValida -- si no seguimos pidiendo la fecha hasta que sea correcta
    -- función que nos ayuda a validar que las fechas de inicio no sea menor que hoy y que la fecha final no sea menor a la fecha de inicio
    validarFechas :: Day -> Day -> IO Bool
    validarFechas fechaInicio fechaFinal = do
        hoy <- utctDay <$> getCurrentTime
        return (fechaInicio < fechaFinal && fechaInicio >= hoy) 

    -- pedimos una cantidad esperada a recolectar y no dejamos de pedir hasta que se ingrese correctamente un numero double
    pedirCantidadEsperada :: IO Double
    pedirCantidadEsperada = do
        putStrLn "Escriba la cantidad esperada a recolectar en Kg."
        input <- getLine
        case readMaybe input of
            Just precio -> return precio
            Nothing -> do
                putStrLn "Entrada inválida. Intenta de nuevo."
                pedirPrecio
    -- función para encontrar un trabajador en una lista de trabajadores
    buscarTrabajador :: String -> [Trabajador] -> Maybe Trabajador
    buscarTrabajador cedula = find (\(Trabajador c _ _ _) -> c == cedula) -- si lo encuentra los retornamos
    -- función para obtener una lista de trabajadores, recibe un string y una lista de trabajadores y devuelve la lista de trabajadores
    obtenerTrabajadoresPorCedulas :: String -> [Trabajador] -> [Trabajador] -- cuyas cédulas estaban en el string
    obtenerTrabajadoresPorCedulas cedulas trabajadores =
        let cedulasSeparadas = splitOn "," cedulas
            trabajadoresEncontrados = map (`buscarTrabajador` trabajadores) cedulasSeparadas 
        in catMaybes trabajadoresEncontrados
    -- al igual que las parcelas obtenemos el id de las cosechas más alto y le sumamos uno
    obtenerNuevoIdCosecha :: FilePath -> IO Int
    obtenerNuevoIdCosecha archivo = do
        cosechas <- obtenerCosechas archivo
        return $ case cosechas of
            [] -> 1
            cs -> maximum (map identificadorCosecha cs) + 1
    -- convertimos un string a un entero si es posible
    convertirAEntero :: String -> Maybe Int
    convertirAEntero = readMaybe
    -- obtenemos los datos de la cosecha usando varias de las funciones mencionadas anteriormente
    obtenerDatosCosecha :: IO()
    obtenerDatosCosecha = do
        parcelas <- leerParcelas "parcelas.json"
        mostrarParcelas parcelas
        putStrLn("Ingrese el id de la parcela en la que va a cosechar")
        idParcelaStr <- getLine -- Todo esto son inputs que se le piden al usuario
        let idParcelaMaybe = convertirAEntero idParcelaStr
        putStrLn ("Ingrese la fecha de inicio de la cosecha (Año-Mes-Día)")
        fechaInicio <- pedirFechaValida
        putStrLn ("Ingrese la fecha de finalizacion de la cosecha (Año-Mes-Día)")
        fechaFinal <- pedirFechaValida
        putStrLn("Ingrese el vegetal a cosechar (debe de coincidir con uno de los vegetales de la parcela elegida)")
        vegetal <- getLine
        ts <- obtenerTrabajadores "trabajadores.json" 
        mostrarListaTrabajadores ts
        putStrLn ("Seleccione los trabajadores que quiere contratar para esta cosecha, con su cedula separados por comas\n Los elementos erróneos no serán agregados")
        cedulasTrabajadores <- getLine
        let trabajadores = obtenerTrabajadoresPorCedulas cedulasTrabajadores ts -- obtenemos los trabajadores que trabajaran en la cosecha
        cantidadEsperada <- pedirCantidadEsperada
        fechasCorrectas <- validarFechas fechaInicio fechaFinal --validamos que las fechas sean correctas
        existe <- validarExistencia "cosechas.json" -- verificamos si existe en archivo .json donde guardaremos las cosechas
        if existe then putStrLn("Verificando existencia...")
        else crearArchivoJSON "cosechas.json" -- en caso de no existir lo creamos
        idCosecha <- obtenerNuevoIdCosecha "cosechas.json" -- obtenemos el nuevo id de la cosecha
        if fechasCorrectas -- si las fechas son correctas
            then do
                case idParcelaMaybe of -- si el id de la parcela tiene un formato correcto
                    Just idParcela -> do 
                        let nuevaCosecha = Cosecha idCosecha idParcela trabajadores fechaInicio fechaFinal cantidadEsperada 0 vegetal True
                        cs <- obtenerCosechas "cosechas.json" --creamos una nueva cosecha y obtenemos las cosechas existentes
                        let esDisponible = validarDisponibilidad nuevaCosecha cs -- validamos la disponibilidad de la parcela usando los datos de la nueva cosecha
                        ps <- leerParcelas "parcelas.json"
                        let vegetalCoincide = verificarTipoVegetal (tipoVegetalCosecha nuevaCosecha) (idParcelaCosecha nuevaCosecha) ps -- verificamos si el vegetal coincide con alguno de los vegetales en la cosecha
                        if vegetalCoincide -- si el vegetal coincide
                            then do 
                                putStrLn("El vegetal es el correcto...") 
                                if esDisponible -- si la parcela está disponible
                                    then
                                        if not (null trabajadores) && length trabajadores == 1 -- si el trabajador es exactamete uno solo
                                            then do 
                                                putStrLn("No hay choques de horario...")
                                                putStrLn("Guardando...")
                                                agregarCosechaJSON nuevaCosecha -- entonces guardamos
                                            else do
                                                putStrLn("Tienes que agregar exactamente un trabajador")
                                    else do -- Si no se cumple alguna de las cosas anteriormente mencionadas se da un mensaje respectivo
                                        putStrLn("Hay choques de horario, verifique e intente de nuevo")
                            else do
                                putStrLn("El vegetal no coincide con el de la parcela. verifique e intente de nuevo")
                    Nothing -> do
                        putStrLn "El código de parcela no es un número válido."
                        obtenerDatosCosecha
        else do
            putStrLn("Las fechas ingresadas son incorrectas, verifíquelas e intente de nuevo")
    -- recibimos el nombre del vegetal el codigo de la parcela y las parcelas
    verificarTipoVegetal :: String -> Int -> [Parcela] -> Bool
    verificarTipoVegetal vegetal codigoP parcelas =
        case find (\p -> codigoParcela p == codigoP) parcelas of -- donde el codigo de la parcela coincide
            Just parcela -> any (\v -> tipoVegetal v == vegetal) (vegetales parcela) --  si algún nombre de vegetal coincide con el vegetal enonces retornamos true
            Nothing -> False -- si ninguno coincide retornamos false

    -- validamos la disponibilidad de la parcela
    validarDisponibilidad :: Cosecha -> [Cosecha] -> Bool
    validarDisponibilidad cosechaNueva cosechas =
        all fechasNoChocan cosechasFiltradas -- verificacmos que no haya una cosecha en las mismas fechas y en la misma parcela
        where
            idParcelaNueva = idParcelaCosecha cosechaNueva
            fechaInicioNueva = fechaInicio cosechaNueva
            fechaFinalNueva = fechaFinal cosechaNueva

            cosechasFiltradas = filter (\c -> estadoCosecha c &&
                                         idParcelaCosecha c == idParcelaNueva) cosechas
            fechasNoChocan cosechaExistente =
                fechaInicioNueva > fechaFinal cosechaExistente || 
                fechaFinalNueva < fechaInicio cosechaExistente 

    -- función para obtener las cosechas
    obtenerCosechas :: FilePath -> IO [Cosecha]
    obtenerCosechas archivo = do
        existe <- validarExistencia archivo  
        if existe
            then do
                contenido <- BL.readFile archivo
                case eitherDecode contenido of
                    Right hs -> return hs
                    Left _ -> return []
            else do
                crearArchivoJSON archivo
                return []
    -- función para guardar las cosechas en una lista
    guardarCosechas :: [Cosecha] -> IO()
    guardarCosechas cs = do
        BL.writeFile "cosechas.json" (encode cs)
    -- función que recibe una cosecha y la adjunta a la lista de cosechas existentes
    -- luego guarda nuevamente el contenido
    agregarCosechaJSON :: Cosecha -> IO()
    agregarCosechaJSON cosecha = do
        cs <- obtenerCosechas "cosechas.json"
        let listaActualizada = cs ++ [cosecha]
        guardarCosechas listaActualizada
    -- muestra sólo una cosecha
    mostrarCosechaSola :: Cosecha -> IO ()
    mostrarCosechaSola cosecha@(Cosecha idCosecha idParcela trabajadores fInicio fFinal esperada obtenida vegetal estado) = do
        putStrLn encabezado
        putStrLn lineaSeparadora
        imprimirFila cosecha
        putStrLn "\nTrabajadores asignados:"
        putStrLn trabajadoresEncabezado
        putStrLn trabajadoresLinea
        mapM_ imprimirTrabajador trabajadores
        where -- imprimimos los datos de la cosecha
            encabezado = printf "%-5s | %-10s | %-10s | %-12s | %-12s | %-10s | %-10s | %-10s | %-6s"
                    "ID" "Parcela" "Trabaj." "Inicio" "Final" "Esperada" "Obtenida" "Vegetal" "Activa"
            lineaSeparadora = replicate 100 '-' 
            formatearFecha = formatTime defaultTimeLocale "%Y-%m-%d"

            imprimirFila (Cosecha idCosecha idParcela trabajadores fInicio fFinal esperada obtenida vegetal estado) =
                printf "%-5d | %-10d | %-10d | %-12s | %-12s | %-10.2f | %-10.2f | %-10s | %-6s\n"
                idCosecha
                idParcela
                (length trabajadores)
                (formatearFecha fInicio)
                (formatearFecha fFinal)
                esperada
                obtenida
                vegetal
                (if estado then "Sí" else "No")

            trabajadoresEncabezado = printf "%-15s | %-20s" "Nombre" "Rol"
            trabajadoresLinea = replicate 40 '-'

            imprimirTrabajador (Trabajador cedula nombre rol cantidadCosechasTrabajadas) =
                printf " %-20s | %-20s | %-10s | %-10s\n" cedula nombre rol (show cantidadCosechasTrabajadas)
    -- función para imprimir todas las cosechas en una lista
    mostrarCosechas :: [Cosecha] -> IO ()
    mostrarCosechas cosechas = do
        putStrLn encabezado
        putStrLn lineaSeparadora
        mapM_ imprimirFila cosechas
        where
            encabezado = printf "%-5s | %-10s | %-10s | %-12s | %-12s | %-10s | %-10s | %-10s | %-6s"
                        "ID" "Parcela" "Trabaj." "Inicio" "Final" "Esperada" "Obtenida" "Vegetal" "Activa"
            lineaSeparadora = replicate 100 '-'
            imprimirFila (Cosecha idCosecha idParcela trabajadores fInicio fFinal esperada obtenida vegetal estado) =
                printf "%-5d | %-10d | %-10d | %-12s | %-12s | %-10.2f | %-10.2f | %-10s | %-6s\n"
                    idCosecha
                    idParcela
                    (length trabajadores)
                    (formatearFecha fInicio)
                    (formatearFecha fFinal)
                    esperada
                    obtenida
                    vegetal
                    (if estado then "Sí" else "No")

            formatearFecha = formatTime defaultTimeLocale "%Y-%m-%d"
    -- buscamos una cosecha y retornamos si se encuentra la cosecha donde el id coincida
    buscarCosechaPorId :: Int -> [Cosecha] -> Maybe Cosecha
    buscarCosechaPorId idBuscado cosechas =
        find (\c -> identificadorCosecha c == idBuscado) cosechas
    -- funcion para ser usada en el modulo de menu cosechas, recibe el codigo de la cosecha que se quiere buscar
    mostrarCosechaSolaUSER :: String -> IO ()
    mostrarCosechaSolaUSER codCosechaStr = do
        cs <- obtenerCosechas "cosechas.json"
        let codCosechaMaybe = convertirAEntero codCosechaStr
        case codCosechaMaybe of
            Just codCosecha -> do
                let cosechaImprimirMaybe = buscarCosechaPorId codCosecha cs
                case cosechaImprimirMaybe of
                    Just cosechaImprimir -> mostrarCosechaSola cosechaImprimir
                    Nothing -> putStrLn "Cosecha no encontrada..."
            Nothing -> putStrLn "Formato de número erróneo..."
    -- funcion para mostrar todas las cosechas
    mostrarCosechasUSER :: IO()
    mostrarCosechasUSER = do

        cs <- obtenerCosechas "cosechas.json"
        mostrarCosechas cs
    
{-------------------------}
-- De AQUÍ EN ADELANTE TRABAJAREMOS CON CIERRE DE COSECHAS.
    -- función para obtener los vegetales en una parcela
    obtenerVegetalesDeParcela :: Parcela -> [Vegetal]
    obtenerVegetalesDeParcela parcela = vegetales parcela
    -- funcion para actualizar los datos en una parcela
    actualizarParcela :: Int -> String -> Double -> Parcela -> Parcela
    actualizarParcela idParcela vegetal cantidad parcela -- se recibe el id de la parcela, el vegetal, la cantidad recolectada y la parcela
        | codigoParcela parcela == idParcela = -- si el codigo de la parcela y el id ingresado son iguales
            let 
                vegetalCorrespondiente = find (\v -> tipoVegetal v == vegetal) (obtenerVegetalesDeParcela parcela) -- obtenemos el vegetal donde el tipo de vegetal se igual
            in case vegetalCorrespondiente of
                Just v -> 
                    let ingreso = cantidad * precioPorKilo v -- multiplicamos lo recolectado por el precio asociado
                    in parcela { -- actualizamos estoos datos en la parcela
                        historialVenta = historialVenta parcela + ingreso,
                        volumenCosecha = volumenCosecha parcela + cantidad
                    }
                Nothing -> parcela 
        | otherwise = parcela --retornamos la parcela
    
    -- pedimos la cantidad recolectada hasta que se ingrese un formato correcto
    pedirCantidadRecolectada :: IO Double
    pedirCantidadRecolectada = do
        input <- getLine
        case readMaybe input of
            Just precio -> return precio
            Nothing -> do
                putStrLn "Entrada inválida. Intenta de nuevo."
                return 0.0001
    -- actualizamos la cosecha y le cambiamos el estado a false para indicar que está cerrada
    actualizarCosecha :: Int -> Double -> String -> Cosecha -> Cosecha
    actualizarCosecha codigo nuevaCantidad vegetal cosecha
        | identificadorCosecha cosecha == codigo =
            cosecha { produccionObtenida = nuevaCantidad, estadoCosecha = False}
        | otherwise = cosecha
    -- obtenemos el codigo de la parcela a partir de la cosecha
    obtenerCodigoParcelaPorCosecha :: Int -> [Cosecha] -> Maybe Int
    obtenerCodigoParcelaPorCosecha idCosecha cosechas =
        idParcelaCosecha <$> find (\c -> identificadorCosecha c == idCosecha) cosechas
    -- actualizamos los datos tanto en la parcela como en la cosecha
    actualizarDatosEnCosechaYParcela :: Int -> Double -> String -> IO Bool
    actualizarDatosEnCosechaYParcela codigoCosecha cantidadRecolectada vegetalCosechado = do
        cs <- obtenerCosechas "cosechas.json"
        ps <- leerParcelas "parcelas.json" --obtenemos la lista de parcelas y cosechas
        let resultado = obtenerCodigoParcelaPorCosecha codigoCosecha cs -- tratamos de obtener el codigo de la parcela
        case resultado of
            Just codigoParcela -> do  --  si lo conseguimos entonces
                let cosechasActualizadas = map(actualizarCosecha codigoCosecha cantidadRecolectada vegetalCosechado) cs --actualizamos las cosechas 
                let parcelasActualizadas = map(actualizarParcela codigoParcela vegetalCosechado cantidadRecolectada) ps --actualizamos las parcelas
                guardarCosechas cosechasActualizadas -- guardamos ambas listas de nuevo
                guardarParcelas "parcelas.json" parcelasActualizadas
                return True -- si se guardo exitosamente entonces retornamos true
            Nothing -> return False -- si no se guardó retornamos false
    -- función para aumentar en 1 la cantidad de cosechas realizada por el traajador
    actualizarTrabajadores :: Trabajador -> [Trabajador] -> [Trabajador]
    actualizarTrabajadores trabajadorCosecha trabajadoresGeneral =
        map actualizar trabajadoresGeneral
        where
            actualizar t
                | cedula t == cedula trabajadorCosecha = 
                    t { cantidadCosechasTrabajadas = cantidadCosechasTrabajadas t + 1 }
                | otherwise = t

    -- función para pedir el codigo de la cosecha que se va a cerrar
    pedirCodigoCosecha :: IO()
    pedirCodigoCosecha = do
        putStrLn ("Ingrese el codigo (ID) de la cosecha que desea cerrar")
        codigoStr <- getLine --pedimos el codigo
        putStrLn("Indique la cantidad recolectada en Kg")
        cantidadRecolectada <- pedirCantidadRecolectada -- pedimos la cantidad recolectada
        let codigoCosechaMaybe = convertirAEntero codigoStr -- obtenemos el maybe codigo
        if cantidadRecolectada == 0.0001 -- si la cantidad es el monto simbolico para error entonces
            then
                putStrLn("Regresando al menú principal...") -- regresamos al menu principal
            else -- en caso de no ser el monto de error
                case codigoCosechaMaybe of  -- si el codigo maybe
                    Just codigoCosecha -> do -- si resulta ser un numero valid
                        cs <- obtenerCosechas "cosechas.json"
                        let cosechaEncontrada = find (\c -> identificadorCosecha c == codigoCosecha && estadoCosecha c) cs -- se identifica la cosecha que se quiere cerrar
                        case cosechaEncontrada of -- si la cosecha
                            Just cosecha -> do -- resulta ser una cosecha
                                let vegetal = tipoVegetalCosecha cosecha -- obtenemos los datos de la cosecha que vamos a necesitar para actualizar los datos
                                let trabajadoresCosecha = trabajadores cosecha
                                let trabajadorCosecha = head trabajadoresCosecha
                                trabajadoresGeneral <- obtenerTrabajadores "trabajadores.json"
                                let trabajadoresActualizados = actualizarTrabajadores trabajadorCosecha trabajadoresGeneral --actualizamos al trabajador
                                guardarTrabajadores trabajadoresActualizados "trabajadores.json" --guardamos los trabajadores

                                guardado <- actualizarDatosEnCosechaYParcela codigoCosecha cantidadRecolectada vegetal
                                if guardado -- si se guardo exitosamente entonces imprimimos un mensaje que lo indica
                                    then putStrLn "Cosecha cerrada, datos actualizados...."
                                    else putStrLn "Cosecha no cerrada, datos no guardados, verifique de nuevo"
                            Nothing -> putStrLn "Cosecha no encontrada o ya cerrada..."
                    Nothing -> putStrLn "Formato de número inválido"

{------------------------------------------------------------------------}
-- CANCELACION DE COSECHAS 

-- funcion principal para cancelar una cosecha

    cancelarCosecha :: Int -> IO Bool
    cancelarCosecha idCosecha = do
        -- obtiene todas las cosechas del archivo JSON
        cosechas <- obtenerCosechas "cosechas.json"
    
    -- Busca la cosecha que coincida con el ID y esté activa
        case find (\c -> identificadorCosecha c == idCosecha && estadoCosecha c) cosechas of
            Nothing -> return False  -- No se encontró o ya está cerrada
            Just _ -> do
                -- Filtra para eliminar la cosecha específica
                let nuevasCosechas = filter (\c -> identificadorCosecha c /= idCosecha) cosechas
                -- Guarda la lista actualizada en el archivo
                guardarCosechas nuevasCosechas
                return True

    cancelarCosechaUSER :: IO ()
    cancelarCosechaUSER = do
        putStrLn "Ingrese el ID de la cosecha a cancelar:"
        input <- getLine
        case readMaybe input of
            Nothing -> do
                -- Validacion de entrada numérica
                putStrLn "ID inválido. Debe ser un número."
                cancelarCosechaUSER  -- Vuelve a solicitar el dato
            Just idC -> do
                -- Intenta cancelar la cosecha
                resultado <- cancelarCosecha idC
                if resultado
                    then putStrLn $ "Cosecha #" ++ show idC ++ " cancelada exitosamente."
                    else putStrLn $ "No se pudo cancelar la cosecha #" ++ show idC ++ 
                            ". Verifique que existe y no está cerrada."

{------------------------------------------------------------------------}
-- MODIFICACIÓN DE COSECHAS 

-- | Funcion principal para modificar cosechas
-- Guía al usuario a través del proceso de modificación
    modificarCosecha :: IO ()
    modificarCosecha = do
        -- Muestra todas las cosechas para referencia
        mostrarCosechasUSER
    
        putStrLn "Ingrese el ID de la cosecha a modificar:"
        idStr <- getLine
        case convertirAEntero idStr of
            Nothing -> do
                putStrLn "ID inválido. Debe ser un número."
                modificarCosecha
            Just idCosecha -> do
                -- Obtiene datos necesarios
                cosechas <- obtenerCosechas "cosechas.json"
                parcelas <- leerParcelas "parcelas.json"
            
                case buscarCosechaPorId idCosecha cosechas of
                    Nothing -> do
                        putStrLn "No se encontró la cosecha con ese ID."
                        modificarCosecha
                    Just cosecha -> do
                        -- Verifica que la cosecha esté activa  
                        if not (estadoCosecha cosecha)
                            then do
                                putStrLn "No se puede modificar una cosecha cerrada."
                                modificarCosecha
                            else do
                                -- Obtiene la parcela asociada
                                let mParcela = find (\p -> codigoParcela p == idParcelaCosecha cosecha) parcelas
                            
                                case mParcela of
                                    Nothing -> do
                                        putStrLn "Error: No se encontró la parcela asociada a esta cosecha."
                                        modificarCosecha
                                    Just parcela -> do
                                        -- Muestra información actual
                                        putStrLn "\nInformación actual de la cosecha:"
                                        mostrarCosechaSola cosecha
                                    
                                        putStrLn "\n¿Qué desea modificar?"
                                        putStrLn "1- Parcela"
                                        putStrLn "2- Fechas"
                                        putStrLn "3- Tipo de vegetal"
                                        putStrLn "4- Cancelar"
                                        putStr "Seleccione una opción: "
                                        opcion <- getLine
                                    
                                        case opcion of
                                            "1" -> modificarParcelaCosecha idCosecha cosecha parcelas
                                            "2" -> modificarFechasCosecha idCosecha cosecha
                                            "3" -> modificarVegetalCosecha idCosecha cosecha parcela
                                            "4" -> putStrLn "Modificación cancelada."
                                            _ -> do
                                                putStrLn "Opción inválida."
                                                modificarCosecha

    -- | Modifica la parcela asociada a una cosecha
    modificarParcelaCosecha :: Int -> Cosecha -> [Parcela] -> IO ()
    modificarParcelaCosecha idCosecha cosecha parcelas = do
        -- Muestra parcelas disponibles
        putStrLn "\nParcelas disponibles:"
        mostrarParcelas parcelas
    
        putStrLn "\nIngrese el nuevo ID de parcela:"
        idStr <- getLine
        case convertirAEntero idStr of
            Nothing -> do
                putStrLn "ID inválido."
                modificarParcelaCosecha idCosecha cosecha parcelas
            Just nuevaParcelaId -> do
                -- Busca la nueva parcela
                case find (\p -> codigoParcela p == nuevaParcelaId) parcelas of
                    Nothing -> do
                        putStrLn "No existe una parcela con ese ID."
                        modificarParcelaCosecha idCosecha cosecha parcelas
                    Just nuevaParcela -> do
                        -- Verifica compatibilidad del vegetal
                        let vegetalesNuevaParcela = vegetales nuevaParcela
                            vegetalActual = tipoVegetalCosecha cosecha
                    
                        if not (any (\v -> tipoVegetal v == vegetalActual) vegetalesNuevaParcela)
                            then do
                                putStrLn "El vegetal actual no está en la nueva parcela."
                                putStrLn "Vegetales disponibles en la nueva parcela:"
                                mapM_ imprimirVegetal vegetalesNuevaParcela
                                modificarParcelaCosecha idCosecha cosecha parcelas
                            else do
                                -- Valida disponibilidad en la nueva parcela
                                cosechas <- obtenerCosechas "cosechas.json"
                                let nuevaCosecha = cosecha {
                                        idParcelaCosecha = nuevaParcelaId
                                    }
                            
                                if validarDisponibilidad nuevaCosecha (filter (\c -> identificadorCosecha c /= idCosecha) cosechas)
                                    then do
        
                                        let nuevasCosechas = map (\c -> 
                                                if identificadorCosecha c == idCosecha 
                                                    then nuevaCosecha 
                                                    else c) cosechas
                                        guardarCosechas nuevasCosechas
                                        putStrLn "Parcela modificada exitosamente."
                                    else do
                                        putStrLn "La parcela no está disponible en las fechas seleccionadas."
                                        modificarParcelaCosecha idCosecha cosecha parcelas

    -- | Modifica las fechas de una cosecha 
    modificarFechasCosecha :: Int -> Cosecha -> IO ()
    modificarFechasCosecha idCosecha cosecha = do
        -- Solicita nuevas fechas
        putStrLn "\nIngrese la nueva fecha de inicio (AAAA-MM-DD):"
        nuevaInicio <- pedirFechaValida
        putStrLn "Ingrese la nueva fecha de finalización (AAAA-MM-DD):"
        nuevaFin <- pedirFechaValida

        fechasValidas <- validarFechas nuevaInicio nuevaFin
        if not fechasValidas
            then do
                putStrLn "Las fechas ingresadas son inválidas."
                modificarFechasCosecha idCosecha cosecha
            else do
                -- Verifica disponibilidad con las nuevas fechas
                cosechas <- obtenerCosechas "cosechas.json"
                let nuevaCosecha = cosecha {
                        fechaInicio = nuevaInicio,
                        fechaFinal = nuevaFin
                    }
            
                if validarDisponibilidad nuevaCosecha (filter (\c -> identificadorCosecha c /= idCosecha) cosechas)
                    then do
                        -- Actualiza y guarda los cambios
                        let nuevasCosechas = map (\c -> 
                                if identificadorCosecha c == idCosecha 
                                    then nuevaCosecha 
                                    else c) cosechas
                        guardarCosechas nuevasCosechas
                        putStrLn "Fechas modificadas exitosamente."
                    else do
                        putStrLn "La parcela no está disponible en las nuevas fechas."
                        modificarFechasCosecha idCosecha cosecha

    -- | Modifica el vegetal de una cosecha
    modificarVegetalCosecha :: Int -> Cosecha -> Parcela -> IO ()
    modificarVegetalCosecha idCosecha cosecha parcela = do
        -- Muestra vegetales disponibles en la parcela actual
        let vegetalesParcela = vegetales parcela
    
        putStrLn "\nVegetales disponibles en esta parcela:"
        mapM_ imprimirVegetal vegetalesParcela
    
    -- Solicita nuevo vegetal
        putStrLn "\nIngrese el nuevo tipo de vegetal:"
        nuevoVegetal <- getLine
    
    -- Verifica que el vegetal exista en la parcela
        if not (any (\v -> tipoVegetal v == nuevoVegetal) vegetalesParcela)
            then do
                putStrLn "El tipo de vegetal no está disponible en esta parcela."
                modificarVegetalCosecha idCosecha cosecha parcela
            else do
                -- Actualiza el vegetal
                cosechas <- obtenerCosechas "cosechas.json"
                let nuevaCosecha = cosecha {
                        tipoVegetalCosecha = nuevoVegetal
                    }
                    nuevasCosechas = map (\c -> 
                        if identificadorCosecha c == idCosecha 
                            then nuevaCosecha 
                            else c) cosechas
                guardarCosechas nuevasCosechas
                putStrLn "Tipo de vegetal modificado exitosamente."

{-------------------------------------------------------------------------}
--ESTADISTICAS DEL SISTEMA
    -- ordenamos las parcelas por venta y devolvemos las 3 parcelas con mayor venta historica
    parcelasOrdenadasPorVenta :: [Parcela] -> [Parcela]
    parcelasOrdenadasPorVenta = take 3 . sortBy (flip (comparing historialVenta)) -- ordenamos y tomamos los primeros 3 elementos
    -- ordenamos las parcelas por volumen de cosecha y tomamos la primera parcela en la lista
    parcelasOrdenadasPorVolumen :: [Parcela] -> Parcela
    parcelasOrdenadasPorVolumen = head . sortBy (flip (comparing volumenCosecha))
    -- mostramos el top 3 parcelas con mayor venta
    topTresParcelasVentas :: IO()
    topTresParcelasVentas = do
        parcelas <- leerParcelas "parcelas.json"
        let top3ParcelasOrdenadas = parcelasOrdenadasPorVenta parcelas
        mostrarParcelas top3ParcelasOrdenadas --mostramos la lista de parcelas que obtuvimos
    --imprimimos la parcela con mayor volumen
    topParcelaMayorVolumen :: IO()
    topParcelaMayorVolumen = do
        parcelas <- leerParcelas "parcelas.json"
        let parcelaConMayorVolumen = parcelasOrdenadasPorVolumen parcelas
        let parcelaConMayorVolumenImprimir = [parcelaConMayorVolumen] --convertimos en una lista de un sólo elemento
        mostrarParcelas parcelaConMayorVolumenImprimir --porque mostrar parcelas recibe una lista de parcelas
    -- obtenemos una lista de las cosechas que tiene más producción obtenida que la esperada y que también esten terminadas
    cosechasSobreproduccionTerminadas :: [Cosecha] -> [Cosecha]
    cosechasSobreproduccionTerminadas = filter (\c -> not (estadoCosecha c) && produccionObtenida c > produccionEsperada c)
    -- obtenemos una lista de las cosechas que tiene menos producción obtenida que la esperada y que también esten terminadas
    cosechasSubproduccionTerminadas :: [Cosecha] -> [Cosecha]
    cosechasSubproduccionTerminadas = filter (\c -> not (estadoCosecha c) && produccionObtenida c < produccionEsperada c)
    -- mostramos las cosechas con sobre producción
    cosechasSobreproduccion :: IO()
    cosechasSobreproduccion = do
        cosechas <- obtenerCosechas "cosechas.json"
        let cosechasSobreProd = cosechasSobreproduccionTerminadas cosechas
        mostrarCosechas cosechasSobreProd
    -- mostramos las cosechas con subproducción
    cosechasSubproduccion :: IO()
    cosechasSubproduccion = do
        cosechas <- obtenerCosechas "cosechas.json"
        let cosechasSubProd = cosechasSubproduccionTerminadas cosechas
        mostrarCosechas cosechasSubProd
    
    
    -- ordenamos la lista de trabajadores por la cantidad de cosechas realizadas
    trabajadoresOrdenadosPorTrabajo :: [Trabajador] -> Trabajador
    trabajadoresOrdenadosPorTrabajo = head . sortBy (flip (comparing cantidadCosechasTrabajadas)) -- obtenemos el trabajador que encabeza la lista
    -- mostramos el trabajador con más cosechas
    mostrarTrabajadorConMasCosechas :: IO()
    mostrarTrabajadorConMasCosechas = do
        trabajadores <- obtenerTrabajadores "trabajadores.json"
        let trabajadorMayor = trabajadoresOrdenadosPorTrabajo trabajadores
        let mostrarTrabajador = [trabajadorMayor] --convertimos el trabajador en una lista porque
        mostrarListaTrabajadores mostrarTrabajador --mmostrar lista trabajadores sólo recibe listas de trabajadores