class CityPattern:
    def __init__(self, name, description):
        self.name = name
        self.description = description
    def display(self):
        print(f'{self.name}: {self.description}')
class MainStreet(CityPattern):
    def __init__(self, name='Main Street', description='Central communication hub'):
        super().__init__(name, description)
        self.connected_districts = []
    def add_district(self, district):
        self.connected_districts.append(district)
        print(f'Connected {district.name} to {self.name}')
class PublicSquare(CityPattern):
    def __init__(self, name='Public Square', description='Shared space for innovation and strategic discussion'):
        super().__init__(name, description)
    def host_meeting(self):
        print(f'{self.name} is hosting a strategic meeting.')
class Neighborhood(CityPattern):
    def __init__(self, name, description='Cluster of related business units'):
        super().__init__(name, description)
        self.buildings = []
    def add_building(self, building):
        self.buildings.append(building)
        print(f'{building.name} added to {self.name}')
class Building(CityPattern):
    def __init__(self, name, description='Autonomous team or service'):
        super().__init__(name, description)
class EnterpriseCity:
    def __init__(self):
        self.main_street = MainStreet()
        self.public_square = PublicSquare()
        self.neighborhoods = []
    def add_neighborhood(self, neighborhood):
        self.neighborhoods.append(neighborhood)
        self.main_street.add_district(neighborhood)
    def display_city(self):
        print('Enterprise City Structure:')
        self.main_street.display()
        self.public_square.display()
        for n in self.neighborhoods:
            n.display()
            for b in n.buildings:
                b.display()
def main():
    city = EnterpriseCity()
    tech_district = Neighborhood('Tech District', 'Focus on technology and product development')
    ops_district = Neighborhood('Operations District', 'Handles operational and support functions')
    tech_district.add_building(Building('Cloud Services', 'Handles all cloud infrastructure'))
    tech_district.add_building(Building('Data Analytics', 'Performs data analysis and reporting'))
    ops_district.add_building(Building('Customer Support', 'Manages client relationships'))
    ops_district.add_building(Building('Logistics', 'Oversees supply chain and distribution'))
    city.add_neighborhood(tech_district)
    city.add_neighborhood(ops_district)
    city.display_city()
    city.public_square.host_meeting()
if __name__ == '__main__':
    main()