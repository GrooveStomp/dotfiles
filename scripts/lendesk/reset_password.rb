puts "This must be run within the core-api console!"

require 'faker'
ephemeral_token = Utils::Token.generate
reset_password_token = Utils::Token.generate
new_password = Faker::Lorem.characters(16)

u = User.find_by_email('aaron.oman@lendesk.com')
u.reset_password_token = reset_password_token
u.save(validate: false)

redis_id, token = Auth::Base.generate_token_for(u, '127.0.0.1')

REDIS.mapped_hmset(ephemeral_token, token: reset_password_token, user_uuid: u.uuid)
u.password_answers.each(&:clean)
puts "\n \
export NEW_PASSWORD=\"#{new_password}\"\n \
export RESET_PASSWORD_TOKEN=#{reset_password_token}\n \
export EPHEMERAL_TOKEN=#{ephemeral_token}\n \
export TOKEN=#{token}\n \
curl -X POST\\\n \
     -H \"Authorization: Token token=$TOKEN\"\\\n \
     -d \"token=$RESET_PASSWORD_TOKEN&ephemeral_token=$EPHEMERAL_TOKEN&new_password=$NEW_PASSWORD&password_confirmation=$NEW_PASSWORD\"\\\n \
     http://localhost:3000/api/users/change_password_by_token"
