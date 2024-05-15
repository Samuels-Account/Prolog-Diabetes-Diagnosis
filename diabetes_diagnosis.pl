% Expert System for Diabetes Diagnosis
% This expert system assists healthcare professionals in diagnosing diabetes mellitus
% by evaluating patient symptoms, medical history, and diagnostic tests.

% Define module and necessary libraries
:- module(diabetes_diagnosis, [diagnose/1, run_tests/0, dynamic_diagnosis/0]).
:- use_module(library(lists)).

% Declare dynamic predicates
:- dynamic symptom/2.
:- dynamic risk_factor/2.
:- dynamic fasting_blood_sugar/2.
:- dynamic a1c_level/2.

% Patient symptoms
% Facts representing common symptoms associated with diabetes.
symptom(alice, frequent_urination).
symptom(alice, blurred_vision).
symptom(alice, increased_hunger).
symptom(alice, slow_healing_sores).
symptom(alice, fatigue).

% Risk factors
% Facts detailing various risk factors that increase the likelihood of diabetes.
risk_factor(alice, obesity).
risk_factor(alice, sedentary_lifestyle).
risk_factor(alice, family_history).
risk_factor(alice, high_blood_pressure).

% Diagnostic tests
% Facts representing results from common diagnostic tests for diabetes.
fasting_blood_sugar(alice, 135).
a1c_level(alice, 7.0).

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
    format('~w does not have sufficient evidence to confirm a diabetes diagnosis.~n', [Person]),
    recommendation(Person, 'Maintain a healthy lifestyle and consider preventive screening if symptoms persist.').

% Helper predicates to determine diabetes severity
severe_diabetes(Person) :-
    fasting_blood_sugar(Person, FBS),
    a1c_level(Person, A1C),
    FBS > 126,
    A1C >= 6.5.

high_risk_diabetes(Person) :-
    risk_factor(Person, high_blood_pressure),
    cholesterol_level(Person, high).

diabetes(Person) :-
    fasting_blood_sugar(Person, FBS),
    a1c_level(Person, A1C),
    (FBS > 126; A1C >= 6.5).

prediabetes(Person) :-
    fasting_blood_sugar(Person, FBS),
    a1c_level(Person, A1C),
    FBS =< 126,
    FBS >= 100,
    A1C < 6.5,
    A1C >= 5.7.

% Recommendations based on diagnosis
recommendation(Person, Message) :-
    format('Recommendation for ~w: ~w~n', [Person, Message]).

% Test cases
% Test suite to validate the diagnosis rules and logging capabilities.
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

% Logs a diagnosis for further analysis and system evaluation.
log_diagnosis(Person, Diagnosis) :-
    format('Diagnosis logged for ~w: ~w~n', [Person, Diagnosis]).

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
    assert(fasting_blood_sugar(Person, FBS)),
    write('Enter the A1C level: '), read(A1C),
    assert(a1c_level(Person, A1C)),
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
