:- dynamic job/9.

%%% DATABASE - JOBS %%%

% job(ID, Title, InterestArea, TargetCourse, Min_CRA, Min_Semester, Required, Desired, Min_Exp).

job(1, 'BackendDeveloper', 'Back', 'ComputerEngineering', 7.5, 4, 
     ['Python', 'SQL'], ['Docker', 'AWS', 'Git'], 2).

job(2, 'DataAnalyst', 'Data', 'Statistics', 7.0, 5, 
     ['SQL', 'Excel'], ['Python', 'PowerBI', 'Statistics'], 1).

job(3, 'DataScientist', 'Data', 'DataScience', 8.0, 6, 
     ['Python', 'Statistics'], ['MachineLearning', 'R', 'SQL'], 3).

job(4, 'FrontendDeveloper', 'Front', 'InformationSystems', 7.5, 4,
     ['HTML', 'CSS', 'JavaScript'], ['React', 'Vue', 'Figma'], 1).

job(5, 'MachineLearningEngineer', 'Data', 'DataScience', 8.5, 7,
     ['Python', 'MachineLearning'], ['TensorFlow', 'PyTorch', 'Keras'], 4).