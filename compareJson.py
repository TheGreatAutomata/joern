import os
import json
import re
from collections import defaultdict

def load_json(path):
    with open(path, 'r', encoding='utf-8') as file:
        return json.load(file)

def find_matches(dir1, dir2):
    matches = defaultdict(lambda: {'dir1': None, 'dir2': []})
    # Process dir1
    for file in os.listdir(dir1):
        if file.endswith('.json'):
            functionName = file[:-5]
            matches[functionName]['dir1'] = os.path.join(dir1, file)
    # Process dir2
    pattern = re.compile(r"(.+)-\d+-dependency\.json")
    for file in os.listdir(dir2):
        match = pattern.match(file)
        if match:
            functionName = match.group(1)
            matches[functionName]['dir2'].append(os.path.join(dir2, file))
    return matches

def remove_semicolon(s):
    # 如果字符串以分号结尾，则去掉它
    if s.endswith(';'):
        return s[:-1]
    return s

def extract_cpp_identifier(s):
    # 定义C++中常见的限定符和修饰符
    cpp_qualifiers_modifiers = {"const", "struct", "union", "enum", "class", "volatile", "mutable", "extern", "static"}

    # 找到第一个空格的位置
    first_space_index = s.find(' ')

    # 如果没有空格，直接返回原字符串
    if first_space_index == -1:
        return s

    # 获取第一个空格前的单词
    first_word = s[:first_space_index]

    # 检查这个单词是否是限定符或修饰符
    if first_word in cpp_qualifiers_modifiers:
        # 返回第一个空格之后的部分
        return s[first_space_index + 1:]

    # 如果不是限定符或修饰符，返回原字符串
    return s

def extract_after_last_slash(s):
    # 找到最后一个斜杠的位置
    last_slash_index = s.rfind('/')

    # 如果找到斜杠，返回斜杠之后的部分
    if last_slash_index != -1:
        return s[last_slash_index + 1:]

    # 如果没有斜杠，返回整个字符串
    return s

def compare_elements(list1, list2, key, key2=None):
    # Creating maps based on key for both lists
    if key2 is None:
        map1 = {remove_semicolon(item[key]): item for item in list1}
        map2 = {remove_semicolon(item[key]): item for item in list2}
    else:
        map1 = {remove_semicolon(item[key])+"-"+str(item[key2]): item for item in list1}
        map2 = {remove_semicolon(item[key])+"-"+str(item[key2]): item for item in list2}
    common_keys = set(map1.keys()) & set(map2.keys())
    differences = []
    for k in common_keys:
        sub_diff = compare_json(map1[k], map2[k], map1[k])
        if sub_diff:
            differences.extend([f"{k}: {sub_diff}"])
    # Check for extra items in both lists
    extra_in_1 = set(map1.keys()) - set(map2.keys())
    extra_in_2 = set(map2.keys()) - set(map1.keys())
    if extra_in_1:
        differences.append(f"Extra in dir1: {extra_in_1}")
    if extra_in_2:
        differences.append(f"Extra in dir2: {extra_in_2}")
    return differences

def compare_json(json1, json2, template, functionName=None):
    differences = []
    for key, value in template.items():
        if key in ["source_end_line", "type_id", "base_type_id", "code"]:
            continue  # Skip comparing these fields
        if key == "functionName":
            key = functionName
        if isinstance(value, dict):
            if key in json1 and key in json2:
                sub_diff = compare_json(json1[key], json2[key], value, functionName)
                if sub_diff:
                    differences.extend([f"{key}.{sub}" for sub in sub_diff])
            elif key in json1 or key in json2:
                differences.append(key)
        elif isinstance(value, list) and value and isinstance(value[0], dict):
            key_field = 'code' if key == 'reference' else 'name'
            key_field2 = 'line' if key == 'reference' else None
            if json1.get(key) and json2.get(key):
                list_diffs = compare_elements(json1[key], json2[key], key_field, key_field2)
                if list_diffs:
                    differences.extend([f"{key}.{diff}" for diff in list_diffs])
        elif key == "type_name":
            value1 = json1.get(key, value)
            value2 = json2.get(key, value)
            if value1 != value2:
                value1 = extract_cpp_identifier(value1)
                value2 = extract_cpp_identifier(value2)
                if value1 != value2:
                    key2 = "type_alias_name"
                    value2 = json2.get(key2, value)
                    if value1 != value2:
                        value2 = extract_cpp_identifier(value2)
                        if value1 != value2:
                            differences.append(f"{key}(should: {json1.get(key, value)}, but: {json2.get(key, value)})")
        elif key == "filename":
            if extract_after_last_slash(json1.get(key, value)) != extract_after_last_slash(json2.get(key, value)):
                differences.append(f"{key}(should: {json1.get(key, value)}, but: {json2.get(key, value)})")
        else:
            if json1.get(key, value) != json2.get(key, value):
                differences.append(f"{key}(should: {json1.get(key, value)}, but: {json2.get(key, value)})")
    return differences

def process_comparison(matches, template):
    for functionName, files in matches.items():
        if files['dir1'] is None:
            continue  # Skip if no corresponding JSON in dir1

        log_path = os.path.join(os.path.dirname(files['dir1']), f"{functionName}Out.log")
        with open(log_path, 'w', encoding='utf-8') as log:
            if len(files['dir2']) != 1:
                if not files['dir2']:
                    log.write(f"No matching file in dir2 for {functionName}\n")
                else:
                    log.write(f"Multiple matches found in dir2 for {functionName}: {', '.join(files['dir2'])}\n")
            else:
                json1 = load_json(files['dir1'])
                json2 = load_json(files['dir2'][0])
                differences = compare_json(json1, json2, template, functionName)
                log.write(f"Differences: \n")
                for i in differences:
                    log.write(f"{i}\n")


def main(dir1, dir2):
    template = {
        "functionName": {
            "source_end_line": -1,
            "source_beg_line": -1,
            "filename": "",
            "param_list": [
                {
                    "name": "",
                    "type_info": {
                        "kind": "",
                        "size": -1,
                        "type_name": "",
                        "type_alias_name": "",
                        "type_id": "",
                        "filename": "",
                        "source_beg_line": -1,
                        "source_end_line": -1,
                        "base_type_id": -1,
                        "index_in_base": -1,
                        "offset_in_base": -1
                    }
                }
            ],
            "param_variable": [
                {
                    "attributes": {
                        "is_modified": False,
                        "used_in_branch": False,
                        "used_in_index": False
                    },
                    "name": "",
                    "type_info": {
                        "kind": "",
                        "size": -1,
                        "type_name": "",
                        "type_alias_name": "",
                        "type_id": "",
                        "filename": "",
                        "source_beg_line": -1,
                        "source_end_line": -1,
                        "base_type_id": -1,
                        "index_in_base": -1,
                        "offset_in_base": -1
                    }
                }
            ],
            "reference": [
                {
                    "code": "",
                    "line": -1,
                    "filename": "",
                    "variable": "",
                    "is_malloc": False
                }
            ]
        }
    }
    matches = find_matches(dir1, dir2)
    process_comparison(matches, template)

if __name__ == '__main__':
    import sys
    if len(sys.argv) != 3:
        print("Usage: python script.py <directory1> <directory2>")
    else:
        main(sys.argv[1], sys.argv[2])
