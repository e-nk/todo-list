import TaskManager (addTask, viewTasks, markComplete, deleteTask, editTask, filterByCompletion)
import FileManager (saveTasks, loadTasks)
import Task (Task(..), newTask)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

main :: IO ()
main = do
    -- Load existing tasks from a file
    tasks <- loadTasks "tasks.txt"
    mainLoop tasks

mainLoop :: [Task] -> IO ()
mainLoop tasks = do
    putStrLn "Options: [add, view, view-incomplete, complete, edit, delete, exit]"
    option <- getLine
    case option of
        "add" -> do
            putStrLn "Enter task description:"
            desc <- getLine
            putStrLn "Enter priority (High, Medium, Low):"
            priority <- getLine
            putStrLn "Enter due date (YYYY-MM-DD):"
            dueDate <- getLine
            let newTasks = addTask desc priority dueDate tasks
            saveTasks "tasks.txt" newTasks
            putStrLn "Task added!"
            mainLoop newTasks
        "view" -> do
            putStrLn (viewTasks tasks)
            mainLoop tasks
        "view-incomplete" -> do
            let incompleteTasks = filterByCompletion False tasks
            putStrLn (viewTasks incompleteTasks)
            mainLoop tasks
        "complete" -> do
            putStrLn "Enter task ID to mark as complete:"
            idStr <- getLine
            case readMaybe idStr of
                Just taskId -> 
                    let updatedTasks = markComplete tasks taskId
                    in do
                        saveTasks "tasks.txt" updatedTasks
                        putStrLn "Task marked as complete!"
                        mainLoop updatedTasks
                Nothing -> do
                    putStrLn "Invalid task ID."
                    mainLoop tasks
        "edit" -> do
            editTaskPrompt tasks
        "delete" -> do
            deleteTaskPrompt tasks
        "exit" -> putStrLn "Goodbye!"
        _ -> do
            putStrLn "Invalid option."
            mainLoop tasks

-- Edit Task Prompt
editTaskPrompt :: [Task] -> IO ()
editTaskPrompt tasks = do
    putStrLn "Enter Task ID to edit:"
    idStr <- getLine
    case readMaybe idStr of
        Just taskId -> do
            putStrLn "Enter new description:"
            newDesc <- getLine
            let updatedTasks = editTask taskId newDesc tasks
            saveTasks "tasks.txt" updatedTasks
            putStrLn "Task edited!"
            mainLoop updatedTasks
        Nothing -> do
            putStrLn "Invalid task ID."
            mainLoop tasks

-- Delete Task Prompt
deleteTaskPrompt :: [Task] -> IO ()
deleteTaskPrompt tasks = do
    putStrLn "Enter Task ID to delete:"
    idStr <- getLine
    case readMaybe idStr of
        Just taskId -> 
            let updatedTasks = deleteTask taskId tasks
            in do
                saveTasks "tasks.txt" updatedTasks
                putStrLn "Task deleted!"
                mainLoop updatedTasks
        Nothing -> do
            putStrLn "Invalid task ID."
            mainLoop tasks
