# Copyright 2020 KCL-BMEIS - King's College London
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

class CalculateMergedFieldCount:
    def __init__(self, updated_ats):
        self.updated_ats = updated_ats
        self.merged_row_count = 0

    def __call__(self, patient_id, filter_status, start, end):
        for i in range(start + 1, end + 1):
            last_date_str = self.updated_ats[i - 1]
            last_date = (last_date_str[0:4], last_date_str[5:7], last_date_str[8:10])
            cur_date_str = self.updated_ats[i]
            cur_date = (cur_date_str[0:4], cur_date_str[5:7], cur_date_str[8:10])
            if last_date == cur_date:
                self.merged_row_count += 1


class MergeAssessmentRows:
    def __init__(self, resulting_fields, created_fields, existing_field_indices):
        print(created_fields.keys())
        print(created_fields['tested_covid_positive_clean'].dtype, created_fields['tested_covid_positive_clean'])
        print(resulting_fields['tested_covid_positive_clean'].dtype, resulting_fields['tested_covid_positive_clean'])
        self.rfindex = 0
        self.resulting_fields = resulting_fields
        self.created_fields = created_fields
        self.existing_field_indices = existing_field_indices

    def populate_row(self, source_fields, source_index):
        for e in self.existing_field_indices:
            self.resulting_fields[e[0]][self.rfindex] = source_fields[e[1]][source_index]
        for ck, cv in self.created_fields.items():
            self.resulting_fields[ck][self.rfindex] = \
                max(self.resulting_fields[ck][self.rfindex], cv[source_index])

    def __call__(self, fields, dummy, start, end):
        # write the first row to the current resulting field index
        prev_date_str = fields[3][start]
        prev_date = (prev_date_str[0:4], prev_date_str[5:7], prev_date_str[8:10])
        self.populate_row(fields, start)

        for i in range(start + 1, end + 1):
            cur_date_str = fields[3][i]
            cur_date = (cur_date_str[0:4], cur_date_str[5:7], cur_date_str[8:10])
            if cur_date != prev_date:
                self.rfindex += 1
            if i % 1000000 == 0 and i > 0:
                print('.')
            self.populate_row(fields, i)
            prev_date = cur_date

        # finally, update the resulting field index one more time
        self.rfindex += 1