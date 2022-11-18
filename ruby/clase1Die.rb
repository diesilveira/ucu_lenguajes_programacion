
def toss_up
    values = Array.new()
    value = ""
    loop do
        value = gets.chomp
        break if value.empty?
        values.append(value)
    end
    puts values.sample
end

class Card
    attr_reader :suit, :rank

    def initialize(suit, rank)
        @suit = suit
        @rank = rank
    end
end

class Deck
    attr_reader :cards

    def initialize(cards)
        @cards = cards
    end

    def shuffle()
        @cards.shuffle!
        self
    end

    def deal(n)
        return Deck.new(@cards.shift(n))
    end

    def peep()
        @cards[0]
    end

    def suitCount(palo)
        count = 0
        @cards.each do |carta|
            if carta.suit == palo
                count += 1
            end
        end
        return count
    end

    def self.full()
        cards = []
        for i in 1..12 do
            for j in ["Espada", "Basto", "Oro", "Copa"] do
                cards.push(Card.new(j, i))
            end
        end
        return Deck.new(cards)
    end
end

# require './clase1.rb' to load on irb
if __FILE__ == $0
    deck = Deck.full()

    puts(deck.peep())
    puts(deck.peep())
    deck.shuffle()
    puts(deck.peep())

    puts(deck.suitCount("Basto"))

end
