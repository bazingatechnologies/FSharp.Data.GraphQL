module Models = 
    type IPet =
        interface
            abstract Name : string
            abstract Weight: int
            abstract Friends: string[]
        end 

    and Dog = 
        {
            Name: string;
            Woofs: bool;
            Weight: int;
            Friends: string[] 
        }
        interface IPet with
            member x.Name = x.Name
            member x.Weight = x.Weight
            member x.Friends = x.Friends

    and Cat = 
        {
            Name: string;
            Meows: bool;
            Weight: int;
            Friends: string[]
        }
        interface IPet with
            member x.Name = x.Name
            member x.Weight = x.Weight
            member x.Friends = x.Friends

    and Pet =
        | DogCase of Dog
        | CatCase of Cat

    and Friend = 
        {
            Name: string;
            Weight: int
        }