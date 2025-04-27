:- dynamic job/9.

%%% DATABASE - JOBS %%%

% job(ID, Title, InterestArea, TargetCourse, Min_CRA, Min_Semester, Required, Desired, Min_Exp).
job(1, 'BackendDeveloper', 'Back', 'CC', 7.5, 4, 
    ['Python', 'SQL'], ['Docker', 'AWS', 'Git'], 2).

job(2, 'DataAnalyst', 'Data', 'CDIA', 7.0, 5, 
    ['SQL', 'Excel'], ['Python', 'PowerBI', 'Statistics'], 1).

job(3, 'DataScientist', 'Data', 'CDIA', 8.0, 6, 
    ['Python', 'Statistics'], ['MachineLearning', 'R', 'SQL'], 3).

job(4, 'FrontendDeveloper', 'Front', 'CC', 7.5, 4,
    ['HTML', 'CSS', 'JavaScript'], ['React', 'Vue', 'Figma'], 1).

job(5, 'MachineLearningEngineer', 'ML', 'CDIA', 8.5, 7,
    ['Python', 'MachineLearning'], ['TensorFlow', 'PyTorch', 'Keras'], 4).

job(6, 'DevOpsEngineer', 'DevOps', 'CE', 8.0, 5,
    ['Docker', 'Kubernetes'], ['AWS', 'Terraform', 'Git'], 2).

job(7, 'EmbeddedSystemsDeveloper', 'Embarcados', 'CE', 7.5, 4,
    ['C', 'C++'], ['Microcontrollers', 'RTOS', 'IoT'], 1).

job(8, 'FullstackDeveloper', 'Back', 'CC', 8.2, 5,
    ['Node.js', 'React'], ['MongoDB', 'Docker', 'Git'], 3).

job(9, 'AIResearcher', 'ML', 'CE', 9.0, 6,
    ['Python', 'DeepLearning'], ['TensorFlow', 'PyTorch', 'NLP'], 2).

job(10, 'DataEngineer', 'Data', 'CDIA', 7.8, 5,
    ['SQL', 'Python'], ['ETL', 'BigData', 'AWS'], 3).
