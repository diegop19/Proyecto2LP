{-# LANGUAGE DeriveGeneric #-}
module ManipulacionDatos (mostrarHerramientasUSER, guardarHerramientasUSER, mostrarListaTrabajadoresUSER, agregarParcela, mostrarParcelasUSER, obtenerDatosCosecha, agregarTrabajador, mostrarCosechasUSER, mostrarCosechaSolaUSER,pedirCodigoCosecha,cancelarCosechaUSER , clearConsole) where
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
    import Data.List (maximumBy)
    import Data.List (nubBy)
    import Data.List (find)
    import qualified Data.ByteString.Lazy.Char8 as BL

    data Herramienta = Herramienta {   
          codigoHerramienta :: String
        , nombreHerramienta :: String
        , descripcionHerramienta :: String
        , tipoHerramienta :: String
    } deriving (Show, Generic)

    instance FromJSON Herramienta
    instance ToJSON Herramienta

    data Trabajador = Trabajador {
          cedula :: String
        , nombreCompleto :: String
        , rol :: String
        , cantidadCosechasTrabajadas :: Int
    }deriving (Show, Generic)

    instance FromJSON Trabajador
    instance ToJSON Trabajador

    data Parcela = Parcela {
          codigoParcela :: Int
        , nombreParcela :: String
        , zona :: String
        , areaEnMetrosCuadrados :: Int
        , tipoVegetal :: String
        , precioPorKilo :: Double
        , herramientas :: [Herramienta]
        , historialVenta :: Double
        , volumenCosecha :: Double
    }deriving (Show, Generic)

    instance FromJSON Parcela
    instance ToJSON Parcela

    data Cosecha = Cosecha {
          identificadorCosecha :: Int
        , idParcelaCosecha :: Int
        , trabajadores :: [Trabajador]
        , fechaInicio :: Day
        , fechaFinal :: Day
        , produccionEsperada :: Double
        , produccionObtenida :: Double
        , tipoVegetalCosecha :: String
        , estadoCosecha :: Bool

    }deriving(Show, Generic)

    instance FromJSON Cosecha
    instance ToJSON Cosecha

    clearConsole :: IO ()
    clearConsole = do
        _ <- system "cls"  -- Ejecuta el comando 'cls' en la consola de Windows
        return ()

--TODA ESTA ZONA ES DE TRABAJADORES Y HERRAMIENTAS
{----------------------------------------------------------------------------}
    
    obtenerTrabajadores :: FilePath -> IO[Trabajador]
    obtenerTrabajadores archivoTrabajadores = do
        contenido <-  BL.readFile archivoTrabajadores
        case eitherDecode contenido of
            Right hs-> return hs
            Left _-> return []

    mostrarListaTrabajadores :: [Trabajador] -> IO ()
    mostrarListaTrabajadores trabajadores = do
        putStrLn encabezado
        putStrLn lineaSeparadora
        mapM_ imprimirFila trabajadores
        where
            encabezado = printf "%-10s | %-35s | %-10s | %-20s"
                                "Cedula" "Nombre" "rol" "Cosechas"
            lineaSeparadora = replicate 75 '-'
            imprimirFila (Trabajador ced nom rol cosechas) =
                printf "%-10s | %-35s | %-10s | %-20s\n" ced nom rol (show cosechas)
    
    crearArchivoJSON :: String -> IO()
    crearArchivoJSON nombreArchivo = do
        BL.writeFile nombreArchivo (BL.pack "[]")
        putStrLn $ "Archivo "++ nombreArchivo ++" Creado exitosamente."
         
    validarExistencia :: String -> IO Bool
    validarExistencia nombreArchivo = doesFileExist nombreArchivo

    guardarTrabajadores :: [Trabajador] -> FilePath -> IO()
    guardarTrabajadores ts archivo = do
        existe <- validarExistencia archivo
        if existe then
            BL.writeFile archivo (encode ts)
            else do
                crearArchivoJSON archivo
                BL.writeFile archivo (encode ts)


    agregarTrabajador :: IO()
    agregarTrabajador = do
        putStrLn("Agregue el numero de cédula del trabajador")
        cedula <- getLine
        putStrLn("Agregue el nombre del trabajador")
        nombre <- getLine
        putStrLn("Agregue el rol del trabajador")
        rol <- getLine
        existe <- validarExistencia "trabajadores.json"
        if existe then putStrLn("Archivo encontrado")
        else crearArchivoJSON "trabajadores.json"
        ts <- obtenerTrabajadores "trabajadores.json"
        let cosechasTrabajadas = 0
        let nuevoTrabajador = Trabajador cedula nombre rol cosechasTrabajadas
        let listaTrabajadores = ts ++ [nuevoTrabajador]
        guardarTrabajadores listaTrabajadores "trabajadores.json"


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

    mostrarListaHerramientas :: [Herramienta] -> IO ()
    mostrarListaHerramientas herramientas = do
        putStrLn encabezado
        putStrLn lineaSeparadora
        mapM_ imprimirFila herramientas
        where
            encabezado = printf "%-10s | %-15s | %-30s | %-10s"
                            "Código" "Nombre" "Descripción" "Tipo"
            lineaSeparadora = replicate 75 '-'
            imprimirFila (Herramienta cod nom desc tipo) =
                printf "%-10s | %-15s | %-30s | %-10s\n" cod nom desc tipo


    convertirHerramienta :: String -> Maybe Herramienta
    convertirHerramienta herramienta = 
        case splitOn "," herramienta of
            [codigo, nombre, descripcion, tipo] -> Just (Herramienta codigo nombre descripcion tipo)
            _ ->
                Nothing

    leerArchivoPlano :: FilePath -> IO[Herramienta]
    leerArchivoPlano ruta = do
        contenido <- readFile ruta
        let lineas = lines contenido
            herramientas = mapM convertirHerramienta lineas
        case herramientas of
            Just hs -> do
                putStrLn("Archivo leido exitosamente")
                putStrLn ("Guardando las herramientas...") 
                return hs
            Nothing -> do
                putStrLn("Error, el archivo tiene alguna(s) lineas mal")
                return []
    
    guardarHerramientasJSON :: FilePath -> [Herramienta] -> IO()
    guardarHerramientasJSON rutaJSON herramientasLeidas = do
        herramientasExistentes <- obtenerHerramientas rutaJSON
        let todasHerramientas = herramientasExistentes ++ herramientasLeidas
            herramientasActualizadas = eliminarDuplicados todasHerramientas
        mostrarListaHerramientas herramientasActualizadas
        BL.writeFile rutaJSON(encode herramientasActualizadas)
        putStrLn ("Herramientas guardadas exitósamente...")

    eliminarDuplicados :: [Herramienta] -> [Herramienta]
    eliminarDuplicados = nubBy (\h1 h2 -> codigoHerramienta h1 == codigoHerramienta h2)


    guardarHerramientasUSER :: FilePath -> FilePath -> IO()
    guardarHerramientasUSER archivoJSON archivoHerramientas = do 
        herramientasNuevas <- leerArchivoPlano archivoHerramientas
        guardarHerramientasJSON archivoJSON herramientasNuevas
        putStrLn("Éxito.")
    
    mostrarHerramientasUSER :: FilePath -> IO()
    mostrarHerramientasUSER rutaJSON = do
        listaHerramientas <- obtenerHerramientas rutaJSON
        mostrarListaHerramientas listaHerramientas


    mostrarListaTrabajadoresUSER :: FilePath -> IO()
    mostrarListaTrabajadoresUSER archivoTrabajadores = do
        existe <- validarExistencia archivoTrabajadores
        if existe then putStrLn("Archivo encontrado")
        else crearArchivoJSON archivoTrabajadores

        listaTrabajadores <- obtenerTrabajadores archivoTrabajadores
        mostrarListaTrabajadores listaTrabajadores  
{------------------------------------------------------------------------------}
--DE AQUÍ EN ADELANTE TRABAJAREMOS CON LA MANIPULACIÓN DE PARCELAS
    buscarHerramienta :: String -> [Herramienta] -> Maybe Herramienta
    buscarHerramienta codigo hs = find (\h -> codigoHerramienta h == codigo) hs

    obtenerHerramientasParaParcela :: String -> [Herramienta] -> [Herramienta]
    obtenerHerramientasParaParcela codigos listaHerramientas = 
        let codigosSeparados = splitOn "," codigos
            herramientasEncontradas = map (`buscarHerramienta` listaHerramientas) codigosSeparados
        in catMaybes herramientasEncontradas

    pedirPrecio :: IO Double
    pedirPrecio = do
        putStrLn "Escriba el precio por Kg."
        input <- getLine
        case readMaybe input of
            Just precio -> return precio
            Nothing -> do
                putStrLn "Entrada inválida. Intenta de nuevo."
                pedirPrecio

    
    pedirArea :: IO Int
    pedirArea = do
        putStrLn "Escriba el area en mts**2"
        input <- getLine
        case readMaybe input of
            Just area -> return area
            Nothing -> do
                putStrLn "Entrada inválida. Intenta de nuevo."
                pedirArea

    leerParcelas :: FilePath -> IO [Parcela]
    leerParcelas archivo = do
        existe <- validarExistencia archivo
        if not existe 
            then crearArchivoJSON archivo
            else putStrLn("")
        contenido <- BL.readFile archivo
        case decode contenido of
            Just parcelas -> return parcelas
            Nothing -> return []

    obtenerNuevoIdParcela :: FilePath -> IO Int
    obtenerNuevoIdParcela archivo = do
        parcelas <- leerParcelas archivo
        return $ case parcelas of
            [] -> 1
            ps -> maximum (map codigoParcela ps) + 1
            
    guardarParcelas :: FilePath -> [Parcela] -> IO ()
    guardarParcelas archivo parcelas = do
        existe <- validarExistencia archivo
        if existe then
            BL.writeFile archivo (encode parcelas)
            else do
                crearArchivoJSON archivo
                BL.writeFile archivo (encode parcelas)

    crearParcela :: String -> String -> String -> Double -> [Herramienta] -> Int -> IO ()
    crearParcela nombreParcela zonaParcela tipoVegetal precioPorKilo listaHerramientas areaEnMetrosCuadrados = do
        existe <- validarExistencia "parcelas.json"
        if not existe
            then crearArchivoJSON "parcelas.json"
            else putStrLn "Existente"

        listaParcelas <- leerParcelas "parcelas.json"
        id <- obtenerNuevoIdParcela "parcelas.json"

        let nuevaParcela = Parcela id nombreParcela zonaParcela areaEnMetrosCuadrados tipoVegetal precioPorKilo listaHerramientas 0 0
        let parcelasActualizadas = listaParcelas ++ [nuevaParcela]
        guardarParcelas "parcelas.json" parcelasActualizadas

    agregarParcela :: IO()
    agregarParcela = do
        putStrLn("Escriba el nombre de la parcela.")
        nombre <- getLine
        putStrLn("Escriba la zona de la parcela.")
        zona <- getLine
        area <- pedirArea
        putStrLn("Escriba el tipo de vegetal de esta parcela.")
        tipoVegetal <- getLine
        precio <- pedirPrecio
        mostrarHerramientasUSER "herramientas.json"
        putStrLn("Ingrese el código de las herramientas que quiere agregar separados por comas. \n (Los datos erróneos no serán agregados)")
        listaCodigos <- getLine
        listaHerramientas <- obtenerHerramientas "herramientas.json"
        let herramientasParcela = obtenerHerramientasParaParcela listaCodigos listaHerramientas
        nuevoId <- obtenerNuevoIdParcela "parcelas.json"
        if null herramientasParcela then do
            putStrLn("Debes agregar al menos una herramienta para la parcela")
            agregarParcela
        else
            crearParcela nombre zona tipoVegetal precio herramientasParcela area

    mostrarParcelasUSER :: IO()
    mostrarParcelasUSER = do
        parcelas <- leerParcelas "parcelas.json"
        mostrarParcelas parcelas


    mostrarParcelas :: [Parcela] -> IO ()
    mostrarParcelas parcelas = do
        if null parcelas
        then putStrLn "No hay parcelas registradas."
        else mapM_ imprimirParcela parcelas

    imprimirParcela :: Parcela -> IO ()
    imprimirParcela (Parcela id nombre zona area vegetal precio herramientas totalVenta totalCosechado) = do
        putStrLn $ "ID DE LA PARCELA " ++ show id
        putStrLn $ "Nombre de la parcela: " ++ nombre
        putStrLn $ "Zona: " ++ zona
        putStrLn $ "Área (m²): " ++ show area
        putStrLn $ "Tipo de vegetal: " ++ vegetal
        printf "Total recaudado (en colones): %.2f\n" totalVenta
        printf "Total cosechado (en Kg): %.2f\n" totalCosechado
        printf "Precio por Kg: %.2f\n" precio
        putStrLn "Herramientas asociadas:"
        if null herramientas
        then putStrLn "  No hay herramientas asociadas."
        else mapM_ imprimirHerramienta herramientas
        putStrLn $ replicate 40 '-'  -- Separador entre parcelas

    imprimirHerramienta :: Herramienta -> IO ()
    imprimirHerramienta (Herramienta codigo nombre descripcion tipo) = do
        printf "  Código: %s, Nombre: %s, Descripción: %s, Tipo: %s\n" codigo nombre descripcion tipo
{-------------------------------------------------------------------------------------------}
--De aquí en adelante se trabajarán las cosechas
    validarFormatoFecha :: String -> Maybe Day
    validarFormatoFecha fechaStr =
        parseTimeM True defaultTimeLocale "%Y-%m-%d" fechaStr

    pedirFechaValida :: IO Day
    pedirFechaValida = do
        input <- getLine
        case validarFormatoFecha input of
            Just fecha -> return fecha
            Nothing -> do
                putStrLn "Formato inválido. Intente de nuevo (ejemplo: 2025-05-01)."
                pedirFechaValida

    validarFechas :: Day -> Day -> IO Bool
    validarFechas fechaInicio fechaFinal = do
        hoy <- utctDay <$> getCurrentTime
        return (fechaInicio < fechaFinal && fechaInicio >= hoy) 

    
    pedirCantidadEsperada :: IO Double
    pedirCantidadEsperada = do
        putStrLn "Escriba la cantidad esperada a recolectar en Kg."
        input <- getLine
        case readMaybe input of
            Just precio -> return precio
            Nothing -> do
                putStrLn "Entrada inválida. Intenta de nuevo."
                pedirPrecio

    buscarTrabajador :: String -> [Trabajador] -> Maybe Trabajador
    buscarTrabajador cedula = find (\(Trabajador c _ _ _) -> c == cedula)

    obtenerTrabajadoresPorCedulas :: String -> [Trabajador] -> [Trabajador]
    obtenerTrabajadoresPorCedulas cedulas trabajadores =
        let cedulasSeparadas = splitOn "," cedulas
            trabajadoresEncontrados = map (`buscarTrabajador` trabajadores) cedulasSeparadas
        in catMaybes trabajadoresEncontrados

    obtenerNuevoIdCosecha :: FilePath -> IO Int
    obtenerNuevoIdCosecha archivo = do
        cosechas <- obtenerCosechas archivo
        return $ case cosechas of
            [] -> 1
            cs -> maximum (map identificadorCosecha cs) + 1
    
    convertirAEntero :: String -> Maybe Int
    convertirAEntero = readMaybe
    
    obtenerDatosCosecha :: IO()
    obtenerDatosCosecha = do
        parcelas <- leerParcelas "parcelas.json"
        mostrarParcelas parcelas
        putStrLn("Ingrese el id de la parcela en la que va a cosechar")
        idParcelaStr <- getLine
        let idParcelaMaybe = convertirAEntero idParcelaStr
        putStrLn ("Ingrese la fecha de inicio de la cosecha (Año-Mes-Día)")
        fechaInicio <- pedirFechaValida
        putStrLn ("Ingrese la fecha de finalizacion de la cosecha (Año-Mes-Día)")
        fechaFinal <- pedirFechaValida
        putStrLn("Ingrese el vegetal a cosechar (debe de coincidir con el vegetal de la parcela elegida)")
        vegetal <- getLine
        ts <- obtenerTrabajadores "trabajadores.json" 
        mostrarListaTrabajadores ts
        putStrLn ("Seleccione los trabajadores que quiere contratar para esta cosecha, con su cedula separados por comas\n Los elementos erróneos no serán agregados")
        cedulasTrabajadores <- getLine
        let trabajadores = obtenerTrabajadoresPorCedulas cedulasTrabajadores ts
        cantidadEsperada <- pedirCantidadEsperada
        fechasCorrectas <- validarFechas fechaInicio fechaFinal
        existe <- validarExistencia "cosechas.json"
        if existe then putStrLn("Verificando existencia...")
        else crearArchivoJSON "cosechas.json"
        idCosecha <- obtenerNuevoIdCosecha "cosechas.json"
        if fechasCorrectas 
            then do
                case idParcelaMaybe of
                    Just idParcela -> do
                        let nuevaCosecha = Cosecha idCosecha idParcela trabajadores fechaInicio fechaFinal cantidadEsperada 0 vegetal True
                        cs <- obtenerCosechas "cosechas.json"
                        let esDisponible = validarDisponibilidad nuevaCosecha cs
                        ps <- leerParcelas "parcelas.json"
                        let vegetalCoincide = verificarTipoVegetal (tipoVegetalCosecha nuevaCosecha) (idParcelaCosecha nuevaCosecha) ps
                        if vegetalCoincide 
                            then do 
                                putStrLn("El vegetal es el correcto...")
                                if esDisponible
                                    then do 
                                        putStrLn("No hay choques de horario...")
                                        putStrLn("Guardando...")
                                        agregarCosechaJSON nuevaCosecha
                                    else do
                                        putStrLn("Hay choques de horario, verifique e intente de nuevo")
                            else do
                                putStrLn("El vegetal no coincide con el de la parcela. verifique e intente de nuevo")
                    Nothing -> do
                        putStrLn "El código de parcela no es un número válido."
                        obtenerDatosCosecha
        else do
            putStrLn("Las fechas ingresadas son incorrectas, verifíquelas e intente de nuevo")
    
    verificarTipoVegetal :: String -> Int -> [Parcela] -> Bool
    verificarTipoVegetal vegetal codigoP parcelas =
        case find (\p -> codigoParcela p == codigoP) parcelas of
            Just parcela -> tipoVegetal parcela == vegetal
            Nothing -> False

    validarDisponibilidad :: Cosecha -> [Cosecha] -> Bool
    validarDisponibilidad cosechaNueva cosechas =
        all fechasNoChocan cosechasFiltradas
        where
            idParcelaNueva = idParcelaCosecha cosechaNueva
            fechaInicioNueva = fechaInicio cosechaNueva
            fechaFinalNueva = fechaFinal cosechaNueva

            cosechasFiltradas = filter (\c -> estadoCosecha c &&
                                         idParcelaCosecha c == idParcelaNueva) cosechas
            fechasNoChocan cosechaExistente =
                fechaInicioNueva > fechaFinal cosechaExistente || 
                fechaFinalNueva < fechaInicio cosechaExistente 


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

    guardarCosechas :: [Cosecha] -> IO()
    guardarCosechas cs = do
        BL.writeFile "cosechas.json" (encode cs)

    agregarCosechaJSON :: Cosecha -> IO()
    agregarCosechaJSON cosecha = do
        cs <- obtenerCosechas "cosechas.json"
        let listaActualizada = cs ++ [cosecha]
        guardarCosechas listaActualizada

    mostrarCosechaSola :: Cosecha -> IO ()
    mostrarCosechaSola cosecha@(Cosecha idCosecha idParcela trabajadores fInicio fFinal esperada obtenida vegetal estado) = do
        putStrLn encabezado
        putStrLn lineaSeparadora
        imprimirFila cosecha
        putStrLn "\nTrabajadores asignados:"
        putStrLn trabajadoresEncabezado
        putStrLn trabajadoresLinea
        mapM_ imprimirTrabajador trabajadores
        where
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

            imprimirTrabajador (Trabajador _ nombre rol _) =
                printf "%-15s | %-20s\n" nombre rol

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
    
    buscarCosechaPorId :: Int -> [Cosecha] -> Maybe Cosecha
    buscarCosechaPorId idBuscado cosechas =
        find (\c -> identificadorCosecha c == idBuscado) cosechas
    
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
    
    mostrarCosechasUSER :: IO()
    mostrarCosechasUSER = do

        cs <- obtenerCosechas "cosechas.json"
        mostrarCosechas cs
    
{-------------------------}
-- De AQUÍ EN ADELANTE TRABAJAREMOS CON CIERRE DE COSECHAS.
    actualizarParcela :: Int -> Double -> Parcela -> Parcela
    actualizarParcela idParcela cantidad parcela
        | codigoParcela parcela == idParcela =
            let ingreso = cantidad * precioPorKilo parcela
            in parcela {
                historialVenta = historialVenta parcela + ingreso,
                volumenCosecha = volumenCosecha parcela + cantidad
            }
        | otherwise = parcela
    
    
    pedirCantidadRecolectada :: IO Double
    pedirCantidadRecolectada = do
        input <- getLine
        case readMaybe input of
            Just precio -> return precio
            Nothing -> do
                putStrLn "Entrada inválida. Intenta de nuevo."
                return 0.0001

    actualizarCosecha :: Int -> Double -> Cosecha -> Cosecha
    actualizarCosecha codigo nuevaCantidad cosecha
        | identificadorCosecha cosecha == codigo =
            cosecha { produccionObtenida = nuevaCantidad, estadoCosecha = False }
        | otherwise = cosecha

    obtenerCodigoParcelaPorCosecha :: Int -> [Cosecha] -> Maybe Int
    obtenerCodigoParcelaPorCosecha idCosecha cosechas =
        idParcelaCosecha <$> find (\c -> identificadorCosecha c == idCosecha) cosechas

    actualizarDatosEnCosechaYParcela :: Int -> Double -> IO Bool
    actualizarDatosEnCosechaYParcela codigoCosecha cantidadRecolectada = do
        cs <- obtenerCosechas "cosechas.json"
        ps <- leerParcelas "parcelas.json"
        let resultado = obtenerCodigoParcelaPorCosecha codigoCosecha cs
        case resultado of
            Just codigoParcela -> do
                let cosechasActualizadas = map(actualizarCosecha codigoCosecha cantidadRecolectada) cs
                let parcelasActualizadas = map(actualizarParcela codigoParcela cantidadRecolectada) ps
                guardarCosechas cosechasActualizadas
                guardarParcelas "parcelas.json" parcelasActualizadas
                return True
            Nothing -> return False


    pedirCodigoCosecha :: IO()
    pedirCodigoCosecha = do
        putStrLn ("Ingrese el codigo (ID) de la cosecha que desea cerrar")
        codigoStr <- getLine
        putStrLn("Indique la cantidad recolectada en Kg")
        cantidadRecolectada <- pedirCantidadRecolectada
        let codigoCosechaMaybe = convertirAEntero codigoStr
        if cantidadRecolectada == 0.0001
            then
                putStrLn("Regresando al menú principal...")
            else
                case codigoCosechaMaybe of
                    Just codigoCosecha -> do
                        guardado <-actualizarDatosEnCosechaYParcela codigoCosecha cantidadRecolectada
                        if guardado 
                            then putStrLn("Cosecha cerrada, datos actualizados....")
                            else putStrLn("Cosecha no cerrada, datos no guardados, verifique de nuevo")
                    Nothing -> do
                        putStrLn("Formato de número inválido")

{------------------------------------------------------------------------}
-- CANCELACION DE COSECHAS 

    cancelarCosecha :: Int -> IO Bool
    cancelarCosecha idCosecha = do
        cosechas <- obtenerCosechas "cosechas.json"
        case find (\c -> identificadorCosecha c == idCosecha && estadoCosecha c) cosechas of
            Nothing -> return False  -- O no encontro la cosecha o ya esta cerrada
            Just _ -> do
                let nuevasCosechas = filter (\c -> identificadorCosecha c /= idCosecha) cosechas
                guardarCosechas nuevasCosechas
                return True


    cancelarCosechaUSER   :: IO ()
    cancelarCosechaUSER   = do
        putStrLn "Ingrese el ID de la cosecha a cancelar:"
        input <- getLine
        case readMaybe input of
            Nothing -> do
                putStrLn "ID inválido. Debe ser un número."
                cancelarCosechaUSER  
            Just idC -> do
                resultado <- cancelarCosecha idC
                if resultado
                    then putStrLn $ "Cosecha #" ++ show idC ++ " cancelada exitosamente."
                    else putStrLn $ "No se pudo cancelar la cosecha #" ++ show idC ++ 
                                ". Verifique que existe y no está cerrada."