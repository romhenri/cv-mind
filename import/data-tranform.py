def convert_txt_to_prolog(input_txt_path, output_pl_path):
    with open(input_txt_path, 'r', encoding='utf-8') as file:
        lines = file.readlines()

    prolog_facts = []
    id_counter = 101

    for line in lines:
        parts = line.strip().split('\t')
        if len(parts) != 7:
            continue

        name, grade, experience, course, projects, skills_str, areas_str = parts

        skills_list = [f"'{skill.strip()}'" for skill in skills_str.split(',')]
        areas_list = [f"'{area.strip()}'" for area in areas_str.split(',')]

        skills = "[" + ",".join(skills_list) + "]"
        areas = "[" + ",".join(areas_list) + "]"

        prolog_fact = (
            f"candidate({id_counter}, '{name}', {float(grade)}, {int(experience)}, "
            f"'{course}', {int(projects)}, {skills}, {areas})."
        )
        prolog_facts.append(prolog_fact)

        id_counter += 1

    with open(output_pl_path, 'w', encoding='utf-8') as out_file:
        out_file.write('\n'.join(prolog_facts))

convert_txt_to_prolog('plan.txt', 'out.pl')
