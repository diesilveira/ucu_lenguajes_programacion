class Card

    attr_reader :suit, :rank
    def initialize(suit, rank)
        @suit =suit
        @rank =rank
    end
end


class Deck

    attr_reader :cards

    def initialize(cards)
        @cards = cards
    end

    def shuffle!()
        cards.shuffle()
        self
    end

    def deal(number)
        return Deck.new(@cards.shift(number))
    end

    def self.full
        cards = []
        palos = ["Espada", "Basto", "Copa", "Oro"]
        palos.each do |palo|
            for i in 1..12
                cards << Card.new(palo, i)
             end
        
        end
        return Deck.new(cards)
    end



end

if __FILE__ == $0
    puts Deck.full.shuffle().deal(3)
end