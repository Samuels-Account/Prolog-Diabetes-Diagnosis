% Symptoms for multiple patients
symptom(john, excessive_thirst).
symptom(john, frequent_urination).
symptom(john, fatigue).
symptom(jane, blurred_vision).
symptom(jane, frequent_urination).
symptom(jane, slow_healing_sores).
symptom(bob, excessive_thirst).
symptom(bob, unexplained_weight_loss).

% Risk Factors for multiple patients
risk_factor(john, obesity).
risk_factor(john, family_history).
risk_factor(jane, age_over_45).
risk_factor(jane, high_blood_pressure).
risk_factor(bob, sedentary_lifestyle).
risk_factor(bob, family_history).

% Diagnostic Test Results for multiple patients
fasting_blood_sugar(john, 140).  % Example: 140 mg/dL
a1c_level(john, 7.5).  % Example: 7.5%
fasting_blood_sugar(jane, 110).  % Example: 110 mg/dL
a1c_level(jane, 6.0).  % Example: 6.0%
fasting_blood_sugar(bob, 130).  % Example: 130 mg/dL
a1c_level(bob, 6.8).  % Example: 6.8%

% Rules to Diagnose Diabetes
diagnose(Person) :-
    % Check for symptoms
    symptom(Person, excessive_thirst),
    symptom(Person, frequent_urination),

    % Check for risk factors
    risk_factor(Person, obesity),
    risk_factor(Person, family_history),

    % Check diagnostic tests
    fasting_blood_sugar(Person, FBS), FBS > 126,
    a1c_level(Person, A1C), A1C > 6.5,

    % Positive diagnosis
    format('~w has diabetes based on symptoms, risk factors, and diagnostic tests.~n', [Person]).

diagnose(Person) :-
    % Alternative rule for different conditions
    symptom(Person, blurred_vision),
    symptom(Person, slow_healing_sores),
    risk_factor(Person, age_over_45),
    risk_factor(Person, high_blood_pressure),
    fasting_blood_sugar(Person, FBS), FBS < 126,
    a1c_level(Person, A1C), A1C < 6.5,

    % Diagnosis indicating a negative result
    format('~w has no significant signs of diabetes based on test results and symptoms.~n', [Person]).

diagnose(Person) :-
    % If none of the above conditions are met
    format('~w does not have sufficient evidence to confirm a diabetes diagnosis.~n', [Person]).
