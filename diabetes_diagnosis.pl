% Expert System for Diabetes Diagnosis
% This expert system assists healthcare professionals in diagnosing diabetes mellitus
% by evaluating patient symptoms, medical history, and diagnostic tests.

% Define module and necessary libraries
:- module(diabetes_diagnosis, [diagnose/1, run_tests/0]).
:- use_module(library(lists)).

% Patient symptoms
% Facts representing common and less common symptoms associated with diabetes.
symptom(alice, frequent_urination).
symptom(alice, blurred_vision).
symptom(alice, increased_hunger).
symptom(alice, slow_healing_sores).
symptom(alice, fatigue).

% Extended risk factors
% Facts detailing various risk factors that increase the likelihood of diabetes.
risk_factor(alice, obesity).
risk_factor(alice, sedentary_lifestyle).
risk_factor(alice, family_history).
risk_factor(alice, high_blood_pressure).

% Diagnostic tests
% Facts representing results from common diagnostic tests for diabetes.
fasting_blood_sugar(alice, 135).
a1c_level(alice, 7.0).

% Cholesterol levels
% Incorporates cholesterol data as part of the risk assessment for diabetes.
cholesterol_level(john, high).
cholesterol_level(jane, normal).
cholesterol_level(bob, high).
cholesterol_level(mary, high).
cholesterol_level(alice, high).

% Blood pressure records
% Additional diagnostic criterion considering the impact of blood pressure on diabetes risk.
blood_pressure(john, '150/95').
blood_pressure(jane, '130/85').
blood_pressure(bob, '160/100').
blood_pressure(mary, '140/90').
blood_pressure(alice, '145/90').

% Risk assessment rules
% Defines rules to assess the diabetes risk based on cholesterol and blood pressure.
diabetes_risk_assessment(Person, 'High Risk') :-
    cholesterol_level(Person, high),
    blood_pressure(Person, BP),
    split_string(BP, "/", "", [Sys, Dia]),
    number_string(Systolic, Sys),
    number_string(Diastolic, Dia),
    (Systolic > 140; Diastolic > 90).

diabetes_risk_assessment(Person, 'Moderate Risk') :-
    cholesterol_level(Person, normal),
    blood_pressure(Person, BP),
    split_string(BP, "/", "", [Sys, Dia]),
    number_string(Systolic, Sys),
    number_string(Diastolic, Dia),
    (Systolic > 130; Diastolic > 85).

% Fuzzy logic for risk factor severity
% Enhances the risk factor assessment using fuzzy logic to evaluate severity based on specific criteria.
fuzzy_risk_factor_severity(obesity, mild).
fuzzy_risk_factor_severity(obesity, severe) :-
    write('This individual has a BMI indicating severe obesity, which greatly increases diabetes risk.').

fuzzy_risk_factor_severity(family_history, moderate) :-
    write('Family history of diabetes moderately increases the risk.').

fuzzy_risk_factor_severity(high_blood_pressure, critical) :-
    write('High blood pressure is a critical risk factor for diabetes.').

evaluate_fuzzy_risk(Person, RiskFactor) :-
    risk_factor(Person, RiskFactor),
    fuzzy_risk_factor_severity(RiskFactor, Severity),
    format('Risk of diabetes due to ~w is ~w.~n', [RiskFactor, Severity]).

% Diagnosis rules
% Implements hierarchical checking of diabetes conditions to provide a nuanced diagnosis based on various factors.
diagnose(Person) :-
    severe_diabetes(Person),
    format('~w has severe diabetes and requires immediate medical attention.~n', [Person]),
    recommendation(Person, 'Consult a diabetes specialist and consider immediate lifestyle and dietary changes.').

diagnose(Person) :-
    high_risk_diabetes(Person),
    format('~w has high-risk diabetes with elevated blood pressure or cholesterol.~n', [Person]),
    recommendation(Person, 'Adopt a strict diet and exercise regimen, and monitor blood sugar levels regularly.').

diagnose(Person) :-
    diabetes(Person),
    format('~w has diabetes based on symptoms and diagnostic tests.~n', [Person]),
    recommendation(Person, 'Consider diet, exercise, and regular blood sugar monitoring.').

diagnose(Person) :-
    prediabetes(Person),
    format('~w has prediabetes and should seek medical advice to prevent progression.~n', [Person]),
    recommendation(Person, 'Consider improving diet, exercise, and regular blood sugar monitoring.').

diagnose(Person) :-
    borderline_case(Person),
    format('~w is in a borderline state; further testing is advised.~n', [Person]),
    recommendation(Person, 'Schedule a comprehensive health checkup and consult with a physician.').

diagnose(Person) :-
    format('~w does not have sufficient evidence to confirm a diabetes diagnosis.~n', [Person]),
    recommendation(Person, 'Maintain a healthy lifestyle and consider preventive screening if symptoms persist.').

% Logging and testing
% Improves the system's logging capabilities to track diagnostics and recommendations, and implements a testing framework.

% Logs a diagnosis for further analysis and system evaluation.
log_diagnosis(Person, Diagnosis) :-
    format('Diagnosis logged for ~w: ~w~n', [Person, Diagnosis]).

% Logs recommendations made by the system.
log_recommendation(Person, Message) :-
    format('Logging recommendation for ~w: ~w~n', [Person, Message]).

% Test cases
% Robust test suite to validate the diagnosis rules and logging capabilities.
run_tests :-
    % Patients are hypothetical scenarios used to test the system's accuracy and responsiveness.
    diagnose_and_log(john),
    diagnose_and_log(jane),
    diagnose_and_log(bob),
    diagnose_and_log(mary),
    diagnose_and_log(alice),
    diagnose_and_log(charlie),
    diagnose_and_log(diana).

% Diagnose and log wrapper
% Wraps the diagnosis process with logging for comprehensive testing.
diagnose_and_log(Person) :-
    (diagnose(Person) -> Diagnosis = 'Successful'; Diagnosis = 'Failed'),
    log_diagnosis(Person, Diagnosis),
    generate_summary(Person).

% Reflective analysis to enhance understanding and learning from each diagnosis.
reflect_on_diagnosis(Person) :-
    (diagnose(Person) ->
        format('Reflection: Diagnosis of ~w was successful.~n', [Person]),
        recommendation(Person, 'Please provide additional details if needed.')
    ; format('Reflection: Diagnosis of ~w faced challenges due to incomplete data.~n', [Person]),
      recommendation(Person, 'Seek medical testing to clarify results.')).

% Presentation and visualization for detailed analysis
% Summarizes the diagnosis, showing symptoms and risk factors involved.
generate_summary(Person) :-
    findall(S, symptom(Person, S), Symptoms),
    findall(R, risk_factor(Person, R), RiskFactors),
    format('Summary for ~w~nSymptoms: ~w~nRisk Factors: ~w~n', [Person, Symptoms, RiskFactors]).

% Dynamic Diagnosis with User Interaction
% Allows real-time data entry and dynamic interaction with the user to personalize the diagnosis process.
dynamic_diagnosis :-
    write('Enter the name of the person: '), read(Person),
    write('Enter the fasting blood sugar level: '), read(FBS),
    write('Enter the A1C level: '), read(A1C),
    assert(fasting_blood_sugar(Person, FBS)),
    assert(a1c_level(Person, A1C)),
    write('Enter blood pressure (e.g., "140/90"): '), read(BP),
    assert(blood_pressure(Person, BP)),
    write('Enter cholesterol level (normal/high): '), read(Cholesterol),
    assert(cholesterol_level(Person, Cholesterol)),
    write('Enter the symptoms (end with "end"): '),
    read_symptoms(Person),
    write('Enter the risk factors (end with "end"): '),
    read_risk_factors(Person),
    (diagnose_and_log(Person) -> true; ambiguous_data_message(Person)),
    offer_summary(Person).

% Handling Ambiguous or Conflicting Data with Detailed Messaging
% Provides a mechanism to alert users when the input data may lead to inconclusive or conflicting diagnoses.
ambiguous_data_message(Person) :-
    format('Warning: Unable to diagnose ~w due to conflicting or missing data.~n', [Person]),
    recommendation(Person, 'Provide more comprehensive information or seek further medical testing.').

% Read and Assert Symptoms
% Facilitates the entry of multiple symptoms, enhancing the system's ability to capture comprehensive patient data.
read_symptoms(Person) :-
    read(Symptom),
    (Symptom == end; assert(symptom(Person, Symptom)), read_symptoms(Person)).

% Read and Assert Risk Factors
% Allows the addition of multiple risk factors to better assess the patient's health risk profile.
read_risk_factors(Person) :-
    read(RiskFactor),
    (RiskFactor == end; assert(risk_factor(Person, RiskFactor)), read_risk_factors(Person)).

% Offer Summary Post-Diagnosis
% Provides an option to review the collected data and the resulting diagnosis, enhancing user understanding and interaction.
offer_summary(Person) :-
    write('Would you like a summary of the diagnosis? (yes/no): '), read(Answer),
    (Answer == yes -> generate_summary(Person); write('Summary not requested.')).


